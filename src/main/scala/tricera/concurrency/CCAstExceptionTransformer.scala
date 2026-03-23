/**
 * Copyright (c) 2026 Hugo Sacilotto. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 *
 * * Neither the name of the authors nor the names of their
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package tricera.concurrency

import concurrent_c._
import concurrent_c.PrettyPrinterNonStatic
import concurrent_c.Absyn._

import scala.collection.mutable.{HashMap => MHashMap, ListBuffer}
import scala.jdk.CollectionConverters._
import scala.collection.mutable.Set
import tricera.Util.FSharpisms
import tricera.ProgVarProxy.Scope.Parameter

class ExceptionTransformException(msg : String) extends Exception(msg)

object CCAstExceptionTransformer {
  private val printer = new PrettyPrinterNonStatic()

  type FuncExceptionTypeData = Set[ListBuffer[Type_specifier]]
  private val catchAllLabel = "_catch_all_"
  private val startOfCatchLabel = "_catch_"
  private val afterCatchLabel = "_after_catch_"

  private val exceptionFlagVarName = "__exception_flag";
  private val exceptionTypeVarName = "__exception_type";
  private val exceptionValueVarName = "__exception_value";
  private val exceptionValueStructName = "ExceptionValue"
  private val exceptionTypeEnumName = "ExceptionType";

  private val getName = new CCAstGetNameVistor

  private def typeLabelName(listDecSpec: ListDeclaration_specifier): String = {
    val str = new StringBuilder
    for (decSpec <- listDecSpec.asScala) {
      str.append(decSpec match {
        case _typ: Type => _typ.type_specifier_ match {
          case t: Tvoid => "void_"
          case t: Tbool => "_Bool_"
          case t: Tchar => "char_"
          case t: Tshort => "short_"
          case t: Tint => "int_"
          case t: Tlong => "long_"
          case t: Tsigned => "signed_"
          case t: Tunsigned => "unsigned_"
          case t: Tstruct => t.struct_or_union_spec_ match {
            case tagType: TagType => "struct_" + tagType.cident_ + "_"
            case _ => throw new ExceptionTransformException("Invalid parameter declaration in catch")
          }
          case t: Tenum => t.enum_specifier_ match {
            case enumVar: EnumVar => "enum_" + enumVar.cident_ + "_"
            case _ => throw new ExceptionTransformException("Invalid parameter declaration in catch")
          }
          case _ => throw new ExceptionTransformException("Not supported type")
        }
        case _ => ""
      })
    }

    str.toString()
  }

  private def createExceptionTypeEnumName(types: ListBuffer[Type_specifier]): String = {
    val str = new StringBuilder
    for (type_ <- types) {
      str.append(type_ match {
        case t: Tbool => "BOOL_"
        case t: Tchar => "CHAR_"
        case t: Tshort => "SHORT_"
        case t: Tint => "INT_"
        case t: Tlong => "LONG_"
        case t: Tsigned => "SIGNED_"
        case t: Tunsigned => "UNSIGNED_"
        case t: Tstruct => t.struct_or_union_spec_ match {
          case tagType: TagType => "STRUCT_" + tagType.cident_ + "_"
          case _ => throw new ExceptionTransformException("Invalid parameter declaration in catch")
        }
        case t: Tenum => t.enum_specifier_ match {
          case enumVar: EnumVar => "ENUM_" + enumVar.cident_ + "_"
          case _ => throw new ExceptionTransformException("Invalid parameter declaration in catch")
        }
        case _ => throw new ExceptionTransformException("Not supported type")
      })
    }

    str.toString()
  }

  private def createExceptionStructField(typeSpec: ListBuffer[Type_specifier]): String = {
    val declSpecList = new ListDeclaration_specifier
    for (t <- typeSpec) {
      declSpecList.add(new Type(t))
    }
    typeLabelName(declSpecList) + "v"
  }

  private def catchBlockLabelName(paramDecl: Parameter_declaration): String = {
    paramDecl match {
      case catchAll: More => catchAllLabel
      case typeAndParam: TypeAndParam =>
        startOfCatchLabel + typeLabelName(typeAndParam.listdeclaration_specifier_) + typeAndParam.declarator_.accept(getName, ())
      case onlyType: OnlyType => startOfCatchLabel + typeLabelName(onlyType.listdeclaration_specifier_)
      case _ => throw new ExceptionTransformException("Invalid parameter declaration in catch")
    }
  }

  private case class ExceptionTypesCollectionResult(
    funcExceptionTypes: Map[String, FuncExceptionTypeData],
    exceptionTypes: Set[ListBuffer[Type_specifier]]
  )

  def transform(program: Program): Program = {
    val collectionResult = collectExceptionTypes(program)
    val exceptionVarTransformedProgram = program.accept(new ExceptionVariableTransformer, None)
    val transformer = new ExceptionTransformer(collectionResult)
    val transformed_program = exceptionVarTransformedProgram.accept(transformer, null);

    println("=== EXCEPTION TRANSFORMED PROGRAM === ")
    println(printer print transformed_program)
    return transformed_program
  }

  private def collectExceptionTypes(program: Program): ExceptionTypesCollectionResult = {
    val buffer = new MHashMap[String, FuncExceptionTypeData]
    val collector = new FuncExceptionTypesCollector(buffer)
    program.accept(collector, null)

    val exceptionTypes = Set[ListBuffer[Type_specifier]]()
    program.accept(new CatchExceptionTypesCollector(exceptionTypes), exceptionTypes)

    for ((_, typeSet) <- buffer) {
      for (typeSpecList <- typeSet) {
        exceptionTypes.add(typeSpecList)
      }
    }

    ExceptionTypesCollectionResult(buffer.toMap, exceptionTypes)
  }

  private def typeOfThrownExp(thrownExp: Exp): ListBuffer[Type_specifier] = {
    // The TriCera preprocessor adds a cast as a type hint for thrown expressions
    thrownExp match {
      case e: Etypeconv => typeNameToListOfTypeSpec(e.type_name_)
      case _ => throw new ExceptionTransformException("Missing cast in thrown expression")
    }
  }

  private def typeNameToListOfTypeSpec(typeName: Type_name): ListBuffer[Type_specifier] = {
    val types = new ListBuffer[Type_specifier]
    typeName match {
      case plain: PlainType => plain.listspec_qual_.asScala.foreach(spec_qual => {
        spec_qual match {
          case typeSpec: TypeSpec => {
            types.addOne(typeSpec.type_specifier_)
          }
          case _ => throw new ExceptionTransformException("Invalid type declaration")
        }
      })
      case _ => throw new ExceptionTransformException("Invalid type declaration")
    }

    types
  }

  private def listDeclSpecToListOfTypeSpec(listDeclSpec: ListDeclaration_specifier): ListBuffer[Type_specifier] = {
    val types = new ListBuffer[Type_specifier]
    listDeclSpec.asScala.foreach(decl => {
      decl match {
        case t: Type => {
          types.addOne(t.type_specifier_)
        }
        case _ => throw new ExceptionTransformException("Invalid type declaration")
      }
    })
    types
  }

  private class CatchExceptionTypesCollector(
    val exceptionTypesBuffer: Set[ListBuffer[Type_specifier]]
  ) extends ComposVisitor[Set[ListBuffer[Type_specifier]]] {
    override def visit(catchStm: Scatch, arg: Set[ListBuffer[Type_specifier]]): Catch_stm = {
      val paramDecl = catchStm.parameter_declaration_
      val block = catchStm.compound_stm_
      val typeSpecList = new ListBuffer[Type_specifier]
      paramDecl match {
        case onlyType: OnlyType => {
          for (declSpec <- onlyType.listdeclaration_specifier_.asScala) {
            declSpec match {
              case t: Type => {
                typeSpecList.addOne(t.type_specifier_)
              }
            }
          }

        }
        case typeAndParam: TypeAndParam => {
          for (declSpec <- typeAndParam.listdeclaration_specifier_.asScala) {
            declSpec match {
              case t: Type => {
                typeSpecList.addOne(t.type_specifier_)
              }
            }
          }
        }
        case _ => {}
      }

      arg.add(typeSpecList)
      block.accept(this, arg)

      catchStm
    }
  }

  private class FuncExceptionTypesCollector(
    val funcExceptionTypesBuffer: MHashMap[String, FuncExceptionTypeData]
  ) extends ComposVisitor[FuncExceptionTypeData] {

    // TODO: Handle function calls

    override def visit(func: NewFunc, arg: FuncExceptionTypeData): NewFunc = {
      val blockStm = func.compound_stm_
      val funcExceptionTypeData = Set[ListBuffer[Type_specifier]]()

      blockStm.accept(this, funcExceptionTypeData)
      val funcName = func.accept(getName, ())
      funcExceptionTypesBuffer.put(funcName, funcExceptionTypeData)

      func
    }

    override def visit(throwExp: Ethrow, arg: FuncExceptionTypeData): Ethrow = {
      val thrownType = typeOfThrownExp(throwExp.exp_)
      arg.addOne(thrownType)
      throwExp
    }
  }

  private class ExceptionVariableTransformer() extends CCAstCopyWithLocation[Option[Parameter_declaration]] {
    override def visit(catchBlock: Scatch, arg: Option[Parameter_declaration]): Catch_stm = {
      val paramDecl = catchBlock.parameter_declaration_
      copyLocationInformation(
        catchBlock,
        new Scatch(
          catchBlock.parameter_declaration_,
          catchBlock.compound_stm_.accept(this, Some(paramDecl))
        )
      )
    }

    override def visit(eVar: Evar, arg: Option[Parameter_declaration]): Exp = {
      arg match {
        case Some(exceptionTypeAndParam: TypeAndParam) => {
          val exceptionVarName = exceptionTypeAndParam.accept(getName, ())
          if (exceptionVarName == eVar.cident_) {
            // Replace with the global exception value
            val exceptionType = exceptionTypeAndParam.listdeclaration_specifier_
            val fieldName = typeLabelName(exceptionType) + "v"
            copyLocationInformation(eVar, new Eselect(new Evar(exceptionValueVarName), fieldName))
          } else {
            eVar
          }
        }
        case _ => eVar
      }
    }
  }

  private class ExceptionTransformer(
    val exceptionTypeData: ExceptionTypesCollectionResult
  ) extends CCAstCopyWithLocation[Any] {
    private val getName = new CCAstGetNameVistor

    private val funcExceptionTypeData = exceptionTypeData.funcExceptionTypes
    private val exceptionTypes = exceptionTypeData.exceptionTypes

    private val setExceptionFlag = new ExprS(
      new SexprTwo(
        new Eassign(
          new Evar(exceptionFlagVarName),
            new Assign,
              new Econst(new Eint("1"))
        )
      )
    )

    private val unsetExceptionFlag = new ExprS(
      new SexprTwo(
        new Eassign(
          new Evar(exceptionFlagVarName),
            new Assign,
              new Econst(new Eint("0"))
        )
      )
    )

    private def globalVarDecl(types: List[Type_specifier], varName: String): Global = {
      val declSpec = new ListDeclaration_specifier
      for (type_ <- types) {
        declSpec.add(new Type(type_))
      }
      val initDecls = new ListInit_declarator
      initDecls.add(new OnlyDecl(new NoPointer(new Name(varName))))

      new Global(new Declarators(declSpec, initDecls, new ListExtra_specifier))
    }

    private def globalStructDecl(structName: String, fields: List[(List[Type_specifier], String)]): Global = {
      val declSpecList = new ListDeclaration_specifier

      val structDecs = new ListStruct_dec
      for ((fieldTypeList, fieldName) <- fields) {
        val specQualList = new ListSpec_qual
        for (type_ <- fieldTypeList) {
          specQualList.add(new TypeSpec(type_))
        }
        val structDeclaratorList = new ListStruct_declarator
        structDeclaratorList.add(new Decl(new NoPointer(new Name(fieldName))))

        structDecs.add(new Structen(specQualList, structDeclaratorList))
      }

      declSpecList.add(new Type(new Tstruct(new Tag(new Struct, structName, structDecs))))

      new Global(new NoDeclarator(declSpecList, new ListExtra_specifier))
    }

    private def globalEnumDecl(enumName: String, variants: List[String]): Global = {
      val declSpecList = new ListDeclaration_specifier
      val enumeratorList = new ListEnumerator
      for (variant <- variants) {
        enumeratorList.add(new Plain(variant))
      }
      declSpecList.add(new Type(new Tenum(new EnumName(enumName, enumeratorList))))

      new Global(new NoDeclarator(declSpecList, new ListExtra_specifier))
    }

    override def visit(p: Progr, arg: Any): Program = {
      val originalProgDecls = p.listexternal_declaration_
      val extDeclarations = new ListExternal_declaration

      // Enum declaration for exception types
      extDeclarations.add(
        globalEnumDecl(
          exceptionTypeEnumName, 
          exceptionTypes.map(t => createExceptionTypeEnumName(t)).toList
        )
      )

      // Struct declaration for exception values
      extDeclarations.add(
        globalStructDecl(
          exceptionValueStructName, 
          exceptionTypes.map(t => (t.toList, createExceptionStructField(t))).toList
        )
      )

      // Global variables for exception information
      extDeclarations.add(
        globalVarDecl(
          List(new Tint()), exceptionFlagVarName
        )
      )
      extDeclarations.add(
        globalVarDecl(
          List(new Tenum(new EnumVar(exceptionTypeEnumName))),
          exceptionTypeVarName
        )
      )
      extDeclarations.add(
        globalVarDecl(
          List(new Tstruct(new TagType(new Struct, exceptionValueStructName))),
          exceptionValueVarName
        )
      )

      for (dec <- originalProgDecls.asScala) {
        extDeclarations.add(dec.accept(this, arg))
      }

      super.visit(p, arg)
      copyLocationInformation(p, new Progr(extDeclarations))
    }

    override def visit(expStm: ExprS, arg: Any): Stm = {
      val new_stm = expStm.expression_stm_ match {
        case nonEmptyExpStm: SexprTwo => nonEmptyExpStm.exp_ match {
          case throwExp: Ethrow => {
            val stmList = new ListStm
            stmList.add(setExceptionFlag)
            stmList.add(new JumpS(new SjumpFour))
            new CompS(new ScompTwo(stmList))
          }
          case _ => expStm
        }
      }
      copyLocationInformation(expStm, new_stm)
    }

    override def visit(compStm: ScompTwo, arg: Any): ScompTwo = {
      val stms = new ListStm

      for (stm <- compStm.liststm_.asScala) {
        stm match {
          case tryStm: TryCatchS => {
            val (tryBlock, catchBlocks) = tryStm.try_stm_ match {
              case tStm: Stry => {
                (tStm.compound_stm_, tStm.listcatch_stm_)
              }
            }

            val catchTypes = new ListParameter_declaration

            // Collect the types from catch handlers
            for ((catchBlock, i) <- catchBlocks.asScala.view.zipWithIndex) {
              catchBlock match {
                case catchStm: Scatch => {
                  val paramDecl = catchStm.parameter_declaration_

                  paramDecl match {
                    case catchAll: More => {
                      // Check if a catch-all handler is used at a wrong position
                      if (i != catchBlocks.size - 1)
                        throw new ExceptionTransformException("Catch-all can only be the last handler")
                    }
                    case _ => {}
                  }
                  catchTypes.add(paramDecl)
                }
              }
            }

            // Transform try block
            val tryStmList = new ListStm
            tryBlock match {
              case empty: ScompOne => {}
              case stmts: ScompTwo => {
                for (stm <- stmts.liststm_.asScala) {
                  stm match {
                    case exprS: ExprS => exprS.expression_stm_ match {
                      case sExprTwo: SexprTwo => sExprTwo.exp_ match {
                        case eThrow: Ethrow => {
                          // Handle throw statements
                          val thrownType = typeOfThrownExp(eThrow.exp_)

                          var typeMatch = false
                          val iter = catchTypes.asScala.iterator

                          // Check the handlers in order for a matching type
                          while (iter.hasNext && !typeMatch) {
                            val catchType = iter.next
                            catchType match {
                              case catchAll: More => {
                                tryStmList.add(setExceptionFlag)
                                tryStmList.add(new JumpS(new SjumpOne(catchAllLabel)))
                                typeMatch = true
                              }
                              case typeAndParam: TypeAndParam => {
                                // Compare type of expression to catch handler's type
                                if (listDeclSpecToListOfTypeSpec(typeAndParam.listdeclaration_specifier_) == thrownType) {
                                  tryStmList.add(setExceptionFlag)
                                  tryStmList.add(new JumpS(new SjumpOne(catchBlockLabelName(catchType))))
                                  typeMatch = true
                                }
                              }
                              case _ => throw new ExceptionTransformException("Invalid type declaration in catch")
                            }
                          }
                        }
                        case _ => { tryStmList.add(stm.accept(this, arg)) }
                      }
                      case _ => { tryStmList.add(stm.accept(this, arg)) }
                    }
                    case _ => { tryStmList.add(stm.accept(this, arg)) }
                  }
                }
              }
            }
            tryStmList.add(new JumpS(new SjumpOne(afterCatchLabel)))
            val newTryBlock = new ScompTwo(tryStmList)
            stms.add(new CompS(newTryBlock))

            // Transform catch handler
            for (catchBlock <- catchBlocks.asScala) {
              val catchStmList = new ListStm
              catchBlock match {
                case catchStm: Scatch => {
                  val paramDecl = catchStm.parameter_declaration_
                  
                  val compoundStm = catchStm.compound_stm_
                  val blockName = catchBlockLabelName(paramDecl)
                  compoundStm match {
                    case empty: ScompOne => {}
                    case sCompTwo: ScompTwo => {
                    
                      // Reset exception flag
                      catchStmList.add(unsetExceptionFlag)

                      val newSComp = sCompTwo.accept(this, arg)
                      newSComp match {
                        case _: ScompOne => {}
                        case newSCompTwo: ScompTwo => {
                          for (stm <- newSCompTwo.liststm_.asScala) {
                            catchStmList.add(stm.accept(this, arg))
                          }
                        }
                      }
                    }
                  }
                  catchStmList.add(new JumpS(new SjumpOne(afterCatchLabel)))
                  val newCatchBlock = new LabelS(new SlabelOne(blockName, new CompS(new ScompTwo(catchStmList))))
                  stms.add(newCatchBlock)
                }
              }

            }

            // Add a label for after the handlers
            stms.add(new LabelS(new SlabelOne(afterCatchLabel, new ExprS(new SexprOne))))

          }
          case _ => {
            stms.add(stm.accept(this, arg))
          }
        }
      }

      copyLocationInformation(compStm, new ScompTwo(stms))
    }
  }
}
