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
import scala.collection.mutable.Stack
import tricera.Util.FSharpisms
import tricera.ProgVarProxy.Scope.Parameter

class ExceptionTransformException(msg : String) extends Exception(msg)

object CCAstExceptionTransformer {
  private val printer = new PrettyPrinterNonStatic()
  type FuncExceptionTypeData = Set[ListBuffer[Type_specifier]]

  private val exceptionFlagVarName = "__exception_flag";
  private val exceptionTypeVarName = "__exception_type";
  private val exceptionValueVarName = "__exception_value";
  private val exceptionValueStructName = "ExceptionValue"
  private val exceptionTypeEnumName = "ExceptionType";

  private val getName = new CCAstGetNameVistor

  private def typeSpecToString(typeSpec: Type_specifier): String = {
    typeSpec match {
      case t: Tvoid => "void"
      case t: Tbool => "_Bool"
      case t: Tchar => "char"
      case t: Tshort => "short"
      case t: Tint => "int"
      case t: Tlong => "long"
      case t: Tsigned => "signed"
      case t: Tunsigned => "unsigned"
      case t: Tstruct => t.struct_or_union_spec_ match {
        case tagType: TagType => "struct" + tagType.cident_
        case _ => throw new ExceptionTransformException("Invalid parameter declaration in catch")
      }
      case t: Tenum => t.enum_specifier_ match {
        case enumVar: EnumVar => "enum" + enumVar.cident_
        case _ => throw new ExceptionTransformException("Invalid parameter declaration in catch")
      }
      case _ => throw new ExceptionTransformException("Not supported type")
    }
  }

  private def typeLabelName(listDecSpec: ListDeclaration_specifier): String = {
    val str = new StringBuilder
    var joiner = ""
    for (decSpec <- listDecSpec.asScala) {
      str.append(joiner)
      joiner = "_"
      str.append(decSpec match {
        case _type: Type => typeSpecToString(_type.type_specifier_)
        case _ => ""
      })
    }
    str.toString()
  }

  private def typeLabelName(types: ListBuffer[Type_specifier]): String = {
    val str = new StringBuilder
    var joiner = ""
    for (type_ <- types) {
      str.append(joiner)
      joiner = "_"
      str.append(typeSpecToString(type_))
    }
    str.toString()
  }

  private def createExceptionTypeEnumName(types: ListBuffer[Type_specifier]): String = {
    typeLabelName(types).toUpperCase()
  }

  private def createExceptionStructField(typeSpec: ListBuffer[Type_specifier]): String = {
    val listDecSpec = new ListDeclaration_specifier
    for (t <- typeSpec) {
      listDecSpec.add(new Type(t))
    }
    typeLabelName(listDecSpec) + "_v"
  }

  private def createExceptionStructField(listDecSpec: ListDeclaration_specifier): String = {
    typeLabelName(listDecSpec) + "_v"
  }

  private def catchBlockLabelName(funcName: String, paramDecl: Parameter_declaration, tryNumberStack: Stack[Int]): String = {
    val str = new StringBuilder
    var joiner = ""

    str.append(paramDecl match {
      case catchAll: More => funcName + "_catch_all_"
      case typeAndParam: TypeAndParam =>
        funcName + "_catch_" + 
        typeLabelName(typeAndParam.listdeclaration_specifier_) + "_" +
        typeAndParam.declarator_.accept(getName, ()) + "_"
      case onlyType: OnlyType => 
        funcName + "_catch_" +
        typeLabelName(onlyType.listdeclaration_specifier_) + "_"
      case _ => throw new ExceptionTransformException("Invalid parameter declaration in catch")
    })

    for (c <- tryNumberStack) {
      str.append(joiner)
      joiner = "_"
      str.append(c.toString())
    }

    str.toString()
  }

  private def afterCatchLabelName(funcName: String, tryNumberStack: Stack[Int]): String = {
    val str = new StringBuilder
    var joiner = ""
    str.append(funcName + "_after_catch" + "_")

    for (c <- tryNumberStack) {
      str.append(joiner)
      joiner = "_"
      str.append(c.toString())
    }

    str.toString()
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
            val fieldName = createExceptionStructField(exceptionType)
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
  ) extends CCAstCopyWithLocation[(String, Stack[ListBuffer[Parameter_declaration]], Stack[Int])] {
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

    private val emptyReturnStm = new JumpS(new SjumpFour)
    private val abortCall = new ExprS(new SexprTwo(new Efunk(new Evar("abort"))))

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

    override def visit(p: Progr, arg: (String, Stack[ListBuffer[Parameter_declaration]], Stack[Int])): Program = {
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

    private def setExceptionType(typeSpecList: ListBuffer[Type_specifier]): ExprS = {
      new ExprS(
        new SexprTwo(
          new Eassign(
            new Evar(exceptionTypeVarName),
            new Assign,
            new Evar(createExceptionTypeEnumName(typeSpecList))
          )
        )
      )
    }

    private def setExceptionValue(typeSpecList: ListBuffer[Type_specifier], exp: Exp): ExprS = {
      new ExprS(
        new SexprTwo(
          new Eassign(
            new Eselect(
              new Evar(exceptionValueVarName),
              createExceptionStructField(typeSpecList)
            ),
            new Assign, exp
          )
        )
      )
    }

    private def gotoStm(labelName: String): JumpS = {
      new JumpS(new SjumpOne(labelName))
    }

    private def emptyLabelStm(labelName: String): LabelS = {
      new LabelS(new SlabelOne(labelName, new ExprS(new SexprOne)))
    }

    override def visit(funcDef: Afunc, arg: (String, Stack[ListBuffer[Parameter_declaration]], Stack[Int])): External_declaration = {
      val funcName = funcDef.accept(getName, ())
      copyLocationInformation(funcDef, new Afunc(funcDef.function_def_.accept(this, (funcName, new Stack[ListBuffer[Parameter_declaration]], new Stack[Int]))))
    }

    override def visit(expStm: ExprS, arg: (String, Stack[ListBuffer[Parameter_declaration]], Stack[Int])): Stm = {
      val (funcName, catchTypesStack, tryNumberStack) = arg

      val newStm = expStm.expression_stm_ match {
        case nonEmptyExpStm: SexprTwo => nonEmptyExpStm.exp_ match {
          case throwExp: Ethrow => {
            val stmList = new ListStm
            val thrownType = typeOfThrownExp(throwExp.exp_)

            // Set global exception variables
            stmList.add(setExceptionFlag)
            stmList.add(setExceptionType(thrownType))
            stmList.add(setExceptionValue(thrownType, throwExp.exp_))

            if (tryNumberStack.size > 0) {
              val catchTypesStackCopy = catchTypesStack.clone()
              val tryNumberStackCopy = tryNumberStack.clone()
              var typeMatch = false

              while (catchTypesStackCopy.size > 0 && !typeMatch) {
                val catchTypes = catchTypesStackCopy.pop()
                val iter = catchTypes.iterator

                // Check the handlers in order for a matching type
                while (iter.hasNext && !typeMatch) {
                  val catchType = iter.next
                  catchType match {
                    case catchAll: More => {
                      stmList.add(gotoStm(catchBlockLabelName(funcName, catchType, tryNumberStackCopy)))
                      typeMatch = true
                    }
                    case typeAndParam: TypeAndParam => {
                      // Compare type of expression to catch handler's type
                      if (listDeclSpecToListOfTypeSpec(typeAndParam.listdeclaration_specifier_) == thrownType) {
                        stmList.add(gotoStm(catchBlockLabelName(funcName, catchType, tryNumberStackCopy)))
                        typeMatch = true
                      }
                    }
                    case onlyType: OnlyType => {
                      if (listDeclSpecToListOfTypeSpec(onlyType.listdeclaration_specifier_) == thrownType) {
                        stmList.add(gotoStm(catchBlockLabelName(funcName, catchType, tryNumberStackCopy)))
                        typeMatch = true
                      }
                    }
                    case _ => throw new ExceptionTransformException("Invalid type declaration in catch")
                  }
                }
                tryNumberStackCopy.pop()
              }
                if (!typeMatch) {
                  // No handler can catch the exception
                  if (funcName == "main") {
                    stmList.add(abortCall)
                  } else {
                    stmList.add(emptyReturnStm)
                  }
                }

                new CompS(new ScompTwo(stmList))
            } else {
                // Throw is not inside a try block
                if (funcName == "main") {
                  stmList.add(abortCall)
                } else {
                  stmList.add(emptyReturnStm)
                }
                new CompS(new ScompTwo(stmList))
              }
            // }
          }
          case _ => expStm
        }
        case _ => expStm
      }

      copyLocationInformation(expStm, newStm)
    }

    override def visit(tryCatchStm: TryCatchS, arg: (String, Stack[ListBuffer[Parameter_declaration]], Stack[Int])): Stm = {
      val (funcName, catchTypesStack, tryNumberStack) = arg
      val stmList = new ListStm
      val catchTypes = new ListBuffer[Parameter_declaration]
      val (tryBlock, catchBlocks) = tryCatchStm.try_stm_ match {
        case tryStm: Stry => (tryStm.compound_stm_, tryStm.listcatch_stm_)
      }

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
            catchTypes.addOne(paramDecl)
          }
        }
      }

      val catchTypesStackClone = catchTypesStack.clone()
      catchTypesStackClone.push(catchTypes)

      // Transform try block
      val tryStmList = new ListStm
      tryBlock match {
        case empty: ScompOne => {}
        case sCompTwo: ScompTwo => {
          val newSComp = sCompTwo.accept(this, (funcName, catchTypesStackClone, tryNumberStack))
          newSComp match {
            case _: ScompOne => {}
            case newSCompTwo: ScompTwo => {
              for (stm <- newSCompTwo.liststm_.asScala) {
                tryStmList.add(stm)
              }
            }
          }
        }
      }
      tryStmList.add(gotoStm(afterCatchLabelName(funcName, tryNumberStack)))
      val newTryBlock = new ScompTwo(tryStmList)
      stmList.add(new CompS(newTryBlock))


      // Transform catch handler
      for (catchBlock <- catchBlocks.asScala) {
        val catchStmList = new ListStm
        catchBlock match {
          case catchStm: Scatch => {
            val paramDecl = catchStm.parameter_declaration_
            
            val compoundStm = catchStm.compound_stm_
            val blockName = catchBlockLabelName(funcName, paramDecl, tryNumberStack)
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
                      catchStmList.add(stm)
                    }
                  }
                }
              }
            }
            catchStmList.add(gotoStm(afterCatchLabelName(funcName, tryNumberStack)))
            val newCatchBlock = new LabelS(new SlabelOne(blockName, new CompS(new ScompTwo(catchStmList))))
            stmList.add(newCatchBlock)
          }
        }

      }

      // Add a label for after the handlers
      stmList.add(emptyLabelStm(afterCatchLabelName(funcName, tryNumberStack)))
      copyLocationInformation(tryCatchStm, new CompS(new ScompTwo(stmList)))
    }

    override def visit(compStm: ScompTwo, arg: (String, Stack[ListBuffer[Parameter_declaration]], Stack[Int])): ScompTwo = {
      val (funcName, catchTypesStack, tryNumberStack) = arg
      val stms = new ListStm
      var tryCount = 0

      for (stm <- compStm.liststm_.asScala) {
        stm match {
          case tryStm: TryCatchS => {
            tryCount += 1
            val tryNumberStackCopy = tryNumberStack.clone()
            tryNumberStackCopy.push(tryCount)
            val transformedTryStm = tryStm.accept(this, (funcName, catchTypesStack, tryNumberStackCopy))

            transformedTryStm match {
              case compS: CompS => compS.compound_stm_ match {
                case sCompTwo: ScompTwo => {
                  for (s <- sCompTwo.liststm_.asScala) {
                    stms.add(s)
                  }
                } 
                case _ => {}
              }
              case _ => throw new ExceptionTransformException("Invalid stm for transformed try stm")
            }
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
