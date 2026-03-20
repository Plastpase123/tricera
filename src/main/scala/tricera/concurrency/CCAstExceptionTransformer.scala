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

class ExceptionTransformException(msg : String) extends Exception(msg)

object CCAstExceptionTransformer {
  private val printer = new PrettyPrinterNonStatic()

  type FuncExceptionTypeData = Set[ListBuffer[Type_specifier]]

  private case class FuncExceptionTypesCollectionResult(
    funcExceptionTypes: Map[String, FuncExceptionTypeData]
  )

  def transform(program: Program): Program = {
    val collectionResult = collectFuncExceptionTypes(program)
    val transformer = new ExceptionTransformer(collectionResult)
    val transformed_program = program.accept(transformer, null);

    println("=== EXCEPTION TRANSFORMED PROGRAM === ")
    println(printer print transformed_program)
    return transformed_program
  }

  private def collectFuncExceptionTypes(program: Program): FuncExceptionTypesCollectionResult = {
    val buffer = new MHashMap[String, FuncExceptionTypeData]
    val collector = new FuncExceptionTypesCollector(buffer)
    program.accept(collector, null)
    FuncExceptionTypesCollectionResult(buffer.toMap)
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

  private class FuncExceptionTypesCollector(
    val funcExceptionTypesBuffer: MHashMap[String, FuncExceptionTypeData]
  ) extends ComposVisitor[FuncExceptionTypeData] {
    private val getName = new CCAstGetNameVistor

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

  private class ExceptionTransformer(
    val funcExceptionTypes: FuncExceptionTypesCollectionResult
  ) extends CCAstCopyWithLocation[Any] {
    private val exceptionFlagVarName = "__exception_flag";
    private val catchAllLabel = "_catch_all_"
    private val startOfCatchLabel = "_catch_"
    private val afterCatchLabel = "_after_catch_"

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

    override def visit(p: Progr, arg: Any): Program = {
      val originalProgDecls = p.listexternal_declaration_
      val extDeclarations = new ListExternal_declaration

      val declspec = new ListDeclaration_specifier
      declspec.add(new Type(new Tint()))
      val initDecls = new ListInit_declarator
      initDecls.add(new OnlyDecl(new NoPointer(new Name(exceptionFlagVarName))))
      val extraSpecifiers = new ListExtra_specifier

      extDeclarations.add(new Global(new Declarators(declspec, initDecls, extraSpecifiers)))

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
                        case _ => { tryStmList.add(stm.accept(this, ())) }
                      }
                      case _ => { tryStmList.add(stm.accept(this, ())) }
                    }
                    case _ => { tryStmList.add(stm.accept(this, ())) }
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
                    case stmts: ScompTwo => {
                    
                      // Reset exception flag
                      catchStmList.add(unsetExceptionFlag)
                      for (stm <- stmts.liststm_.asScala) {
                        catchStmList.add(stm.accept(this, ()))
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

    private def catchBlockLabelName(paramDecl: Parameter_declaration): String = {
      paramDecl match {
        case catchAll: More => catchAllLabel
        case typeAndParam: TypeAndParam => {
          val str = new StringBuilder(startOfCatchLabel)
          for (decSpec <- typeAndParam.listdeclaration_specifier_.asScala) {
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
        case _ => throw new ExceptionTransformException("Invalid parameter declaration in catch")
      }
    }
  }
}