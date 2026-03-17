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
import scala.collection.mutable.Stack
import tricera.Util.FSharpisms

class ExceptionTransformException(msg : String) extends Exception(msg)

object CCAstExceptionTransformer {
  private val printer = new PrettyPrinterNonStatic()

  private val exceptionFlagVarName = "__exception_flag";

  def transform(program: Program): Program = {
    val transformer = new ExceptionTransformer()
    val transformed_program = program.accept(transformer, null);

    println("=== EXCEPTION TRANSFORMED PROGRAM === ")
    println(printer print transformed_program)
    return transformed_program
  }

  private class ExceptionTransformer() extends CCAstCopyWithLocation[Any] {
    override def visit(p: Progr, arg: Any): Program = {
      val originalProgDecls = p.listexternal_declaration_
      val extDeclarations = new ListExternal_declaration

      val declspec = new ListDeclaration_specifier
      declspec.add(new Type(new Tint()))
      val initDecls = new ListInit_declarator
      initDecls.add(new OnlyDecl(new NoPointer(new Name("__exception_flag"))))
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
        case non_empty_exp_stm: SexprTwo => non_empty_exp_stm.exp_ match {
          case throw_exp: Ethrow => {
            val stmList = new ListStm
            stmList.add(
              new ExprS(new SexprTwo(
                new Eassign(
                  new Evar(exceptionFlagVarName),
                  new Assign,
                  new Econst(new Eint("1"))
                  )
                )
              )
            )
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

            // Transform try block
            val tryStmList = new ListStm
            tryBlock match {
              case empty: ScompOne => {}
              case stmts: ScompTwo => {
                for (stm <- stmts.liststm_.asScala) {
                  tryStmList.add(stm.accept(this, ())) // TODO: Handle throw statements
                }
              }
            }
            val newTryBlock = new ScompTwo(tryStmList)
            stms.add(new CompS(newTryBlock))

            // Transform catch handlers
            for ((catchBlock, i) <- catchBlocks.asScala.view.zipWithIndex) {
              val catchStmList = new ListStm
              catchBlock match {
                case catchStm: Scatch => {
                  val paramDecl = catchStm.parameter_declaration_
                  
                  // Check if a catch-all handler is used at a wrong position
                  paramDecl match {
                    case catchAll: More => {
                      if (i != catchBlocks.size - 1)
                        throw new ExceptionTransformException("Catch-all can only be the last handler")
                    }
                    case _ => {}
                  }

                  val compoundStm = catchStm.compound_stm_
                  val blockName = catchBlockLabelName(paramDecl)
                  // val blockName = "foo"
                  compoundStm match {
                    case empty: ScompOne => {}
                    case stmts: ScompTwo => {
                      for (stm <- stmts.liststm_.asScala) {
                        catchStmList.add(stm.accept(this, ()))
                      }
                    }
                  }
                  catchStmList.add(new JumpS(new SjumpOne("_after_catch")))
                  val newCatchBlock = new LabelS(new SlabelOne(blockName, new CompS(new ScompTwo(catchStmList))))
                  stms.add(newCatchBlock)
                }
              }

            }

            // Add a label for after the handlers
            stms.add(new LabelS(new SlabelOne("_after_catch", new ExprS(new SexprOne))))

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
        case catchAll: More => "_catch_all_"
        case typeAndParam: TypeAndParam => {
          val str = new StringBuilder("_catch_")
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
                case t: Tstruct => throw new ExceptionTransformException("Not implemented")
                case t: Tenum => throw new ExceptionTransformException("Not implemented")
                case _ => throw new ExceptionTransformException("Not supported type")
              }
              case _ => ""
            })
          }

          str.toString()
        }
        case _ => throw new ExceptionTransformException("Illegal parameter declaration in catch")
      }
    }
  }
}