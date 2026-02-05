package tricera.concurrency

import concurrent_c._
import concurrent_c.PrettyPrinterNonStatic
import concurrent_c.Absyn._

import scala.collection.mutable.{HashMap => MHashMap, ListBuffer}
import scala.jdk.CollectionConverters._
import scala.collection.mutable.Stack

class ClassTransformException(msg : String) extends Exception(msg)

object CCAstClassTransformer {

  private case class ClassDefInfo(
    className   : String,
    classFuncs  : ListBuffer[Afunc],
    classVars   : ListBuffer[Global]

    // Nested classes?
  )

  private case class ClassDefCollectionResult(
    classDefs : Map[String, ClassDefInfo]
  )

  private val printer = new PrettyPrinterNonStatic()

  def transform(program: Program): Program = {
    println("=== ORIGINAL PROGRAM === ")
    println(printer print program)
    val collectionResult = collectClassDefs(program)
    if (collectionResult.classDefs.isEmpty)
      return program
    val transformer = new ClassTransformer(collectionResult)
  val p = program.accept(transformer, null)

  println("=== TRANSFORMED PROGRAM === ")
  println(printer print p)
  return p
  }

  private def collectClassDefs(program : Program) : ClassDefCollectionResult = {
    val classDefBuffer = new MHashMap[String, ClassDefInfo]
    val collector = new ClassDefCollector(classDefBuffer)
    program.accept(collector, (null, null))

    ClassDefCollectionResult(classDefBuffer.toMap)
  }

  private class ClassDefCollector(
    val classDefBuffer : MHashMap[String, ClassDefInfo]

  ) extends ComposVisitor[(ListBuffer[Afunc], ListBuffer[Global])] {
    private val getName = new CCAstGetNameVistor

    override def visit(c : AClass, arg : (ListBuffer[Afunc], ListBuffer[Global])) : AClass = {
      // Match Ext_dec against Function_defs and Globals
      val decls = (new ListBuffer[Afunc], new ListBuffer[Global])
      val classDecs = c.class_decs_
      val className = c.cident_

      classDecs.accept(this, decls)

      val classFuncs = decls._1
      val classVars = decls._2

      val collectedClassInfo = ClassDefInfo(className, classFuncs, classVars)
      classDefBuffer.put(className, collectedClassInfo)
      c
    }

    override def visit(cdec : ClassDec, decls : (ListBuffer[Afunc], ListBuffer[Global])) : ClassDec = {
      cdec.external_declaration_ match {
        case fun : Afunc => decls._1 += fun
        case glob : Global => decls._2 += glob
      }
    cdec
    }

  }


    private class ClassTransformer(
      val collectedDefs : ClassDefCollectionResult
    ) extends CCAstCopyWithLocation[Any] {
      val getDeclarator = new CCAstGetDeclaratorVistor
      val getParameters = new CCAstGetParametersVistor
      val getFunctionDeclaration = new CCAstGetFunctionDeclarationVistor
      val copyAst = new CCAstCopyVisitor
      val getFuncBody = new CCAstGetFunctionBodyVistor
      val getAnnotations = new CCAstGetFunctionAnnotationVisitor
      val getName = new CCAstGetNameVistor
      val getType = new CCAstGetTypeVisitor
      val classObjBuffer = new MHashMap[String, String]

      // Converts declarations inside of a class into struct declarators, in order to create a struct declaration
      private def createStructen(dec : Dec, listDeclarationSpecifiers : ListDeclaration_specifier) : Structen = {
        val listSpecQual = new ListSpec_qual
        val listStructDeclarator = new ListStruct_declarator
        val listInitDeclarators = {
          if (dec.isInstanceOf[Declarators]) { dec.asInstanceOf[Declarators].listinit_declarator_ }
          else { new ListInit_declarator }
        }
        listInitDeclarators.forEach(init => listStructDeclarator.add(new Decl(init.accept(getDeclarator, ()))))
        listDeclarationSpecifiers.asScala.foreach {
          case ts : Type => listSpecQual.add(new TypeSpec(ts.type_specifier_))
          case tq : SpecProp => listSpecQual.add(new QualSpec(tq.type_qualifier_))
        }

        return new Structen(listSpecQual, listStructDeclarator)
      }

      // Adds a class object pointer to the parameters of a member function
      // of the form "className *this"
      private def createStructParam(className : String) : Parameter_declaration = {
        val listDeclarationSpecifier = new ListDeclaration_specifier
        val t = new Type(new Tstruct(new TagType(new Struct, className)))
        val param = new BeginPointer(new Point, new Name("this"))

        listDeclarationSpecifier.add(t)

        new TypeAndParam(listDeclarationSpecifier, param)
      }

      // Transforms a member function into a global function definition. If the function
      // is a class constructor, '_init' is added as a suffix to the name of the function
      private def createClassAfunc(fun : Afunc, className : String) : External_declaration = {
        val funcDef = fun.function_def_
        val funcDec = funcDef.accept(getFunctionDeclaration, ())

        val (listDecSpecifier, funcName) = {
          if (funcDef.isInstanceOf[ClassInitFunc]) {
            val l = new ListDeclaration_specifier
            l.add(new Type(new Tvoid))
            val fName = fun.accept(getName, ()) ++ "_init"
            (l, fName)
          }
          else { (funcDec._1, fun.accept(getName, ())) }
        }

        val initDec = funcDec._2
        val dec = initDec.accept(getDeclarator, ())
        val params = dec.accept(getParameters, ())
        val body = funcDef.accept(getFuncBody, ())
        val classParam = createStructParam(className)
        params.add(classParam)
        new Afunc(new NewFunc(listDecSpecifier, new NoPointer(new NewFuncDec(new Name(funcName), new AllSpec(params))), body))

      }

      // Transforms the existing AST nodes and adds member functions as global function definitions
      override def visit(p : Progr, arg : Any) : Program = {
        val originalProgDecs = p.listexternal_declaration_
        val extDeclarations = new ListExternal_declaration
        val defBuffer = collectedDefs.classDefs

        for (classDef <- defBuffer.values) {
          for (func <- classDef.classFuncs) {
            extDeclarations.addLast((createClassAfunc(func, classDef.className)).accept(this, ()))
          }
        }
        for (x <- originalProgDecs.asScala)
        {
          extDeclarations.addLast(x.accept(this, arg))
        }
        super.visit(p, arg)
        copyLocationInformation(p, new Progr(extDeclarations))
      }

      // Replaces "/*& class &*/" annotations with "struct"
      override def visit(ctype : Class, arg : Any) : Struct_or_union = {
        copyLocationInformation(ctype, new Struct)
      }

      // Transform class declaration to struct
      override def visit(cls : AClass, arg : Any) : External_declaration = {
        val defBuffer = collectedDefs.classDefs
        defBuffer get cls.cident_ match {
          case Some(classDef) =>
            if (classDef.classVars.size != 0) {
              val listExtraSpecifier = new ListExtra_specifier
              val declarationSpecifier = new ListDeclaration_specifier
              val listStructDec = new ListStruct_dec
              for (x <- classDef.classVars) {
                val d = x.dec_
                d match {
                  case dec : Declarators =>
                    listStructDec.add(createStructen(dec, dec.listdeclaration_specifier_).accept(this, ()))
                  case noDec : NoDeclarator =>
                    listStructDec.add(createStructen(noDec, noDec.listdeclaration_specifier_).accept(this,()))
                  case _ => throw new ClassTransformException(f"Unsupported class declaration at line: ${d.getLineNum()}")
                }
              }
              declarationSpecifier.add(new Type(new Tstruct(new Tag(new Struct, cls.cident_, listStructDec))))
              return copyLocationInformation(cls, new Global(new NoDeclarator(declarationSpecifier, listExtraSpecifier)))
            }
            return cls
          case None => cls
        }
        cls
      }

      // Transforms a call to a member function via a pointer from "Exp->funcName(...)" to "funcName(..., Exp)"
      private def buildCall(argExp : Exp, pointExp : Epoint, original : Exp, hasParams : Boolean): Exp = {
        val funcName = pointExp.cident_
        val param = argExp
        val params = {
          if (!hasParams) {
            new ListExp
          }
          else {
            val orig = original.asInstanceOf[Efunkpar]
            orig.listexp_
          }
        }
        params.add(param)
        val newExp = new Efunkpar(new Evar(funcName), params)
        copyLocationInformation(original, newExp.accept(this, ()))
      }

      // Transforms a call to a member function via a select from "Exp.funcName(...)" to "funcName(..., &Exp)"
      private def buildCall(argExp : Exp, selExp : Eselect, original : Exp, hasParams : Boolean): Exp = {
        val funcName = selExp.cident_
        val param = new Epreop(new Address, argExp)
        val params = {
          if (!hasParams) {
            new ListExp
          }
          else {
            val orig = original.asInstanceOf[Efunkpar]
            val p = new ListExp
            orig.listexp_.forEach(e => p.add(e.accept(this, ())))
            p
          }
        }
        params.add(param.accept(this, ()))
        val newExp = new Efunkpar(new Evar(funcName), params)
        copyLocationInformation(original, newExp)
      }

      // Finds calls to member functions of the form "Exp.funcName()" or "Exp->funcName()"
      // and transforms them.
      override def visit(exp : Efunk, arg : Any): Exp = {
        exp.exp_ match {
          case selExp: Eselect =>
            selExp.exp_ match {
              case target: Eselect => buildCall(target, selExp, exp, false)
              case target: Epoint => buildCall(target, selExp, exp, false)
              case target: Evar => buildCall(target, selExp, exp, false)
              case _ => exp
            }
          case pointExp: Epoint =>
            pointExp.exp_ match {
              case target: Eselect => buildCall(target, pointExp, exp, false)
              case target: Epoint => buildCall(target, pointExp, exp, false)
              case target: Evar => buildCall(target, pointExp, exp, false)
              case _ => exp
            }
              case _ => exp
        }
      }

      // Finds calls to member functions of the form "Exp.funcName(params)" or "Exp->funcName(params)"
      // and transforms them.
      override def visit(exp : Efunkpar, arg : Any): Exp = {
        val newListExp = new ListExp

        // Process existing parameters
        for (p <- exp.listexp_.asScala) { newListExp.add(p.accept(this, ())) }
        val newParams = new Efunkpar(exp.exp_.accept(this, ()), newListExp)

        newParams.exp_ match {
          case selExp: Eselect =>
            selExp.exp_ match {
              case target: Eselect => buildCall(target, selExp, newParams, true)
              case target: Epoint => buildCall(target, selExp, newParams, true)
              case target: Evar => buildCall(target, selExp, newParams, true)
              case _ => newParams
            }
          case pointExp: Epoint =>
            pointExp.exp_ match {
              case target: Eselect => buildCall(target, pointExp, newParams, true)
              case target: Epoint => buildCall(target, pointExp, newParams, true)
              case target: Evar => buildCall(target, pointExp, newParams, true)
              case _ => newParams
            }
              case _ => newParams
        }
      }

      // Breaks calls to class constructors of the form "/*& class &*/ className objname(...)"
      // into a declaration "struct className objName" and a call to "className_init(..., &objName)"
      override def visit(stm : ScompTwo, arg : Any): ScompTwo = {
        val stmts = new ListStm
        for (x <- stm.liststm_.asScala) {
          x match {
            case decS : DecS =>
              decS.dec_ match {
                case decls : Declarators =>
                  val listInitDeclarator = decls.listinit_declarator_
                  for (init <- listInitDeclarator.asScala) {
                    val dirDec = {
                      val dec = init.accept(getDeclarator, ())
                      dec match {
                        case d : BeginPointer => d.direct_declarator_
                        case d : NoPointer => d.direct_declarator_
                      }
                    }
                    dirDec match {
                      case classConstructor : ClassCons =>
                        val types = new ListBuffer[Type_specifier]
                        decls.listdeclaration_specifier_.forEach(dSpec => dSpec.accept(getType, types))
                        val className = {
                          var cn = ""
                          for (t <- types) {
                            t match {
                              case tstruct : Tstruct =>
                                tstruct.struct_or_union_spec_ match {
                                  case tagType : TagType =>
                                    if (tagType.struct_or_union_.isInstanceOf[Class]) {
                                      cn = tagType.cident_
                                    }
                                    else {
                                      throw new ClassTransformException(f"Call to class constructor has wrong class key at line ${dirDec.getLineNum()}")
                                    }
                                  case _ => throw new ClassTransformException(f"Call to class constructor must be of type TagType")
                                }
                              case _ => // do nothing
                            }
                          }
                          if (cn != "") { cn }
                          else { throw new ClassTransformException(f"Class constructor isn't declared with a struct type") }
                        }
                        val lExp = new ListExp
                        val objName = classConstructor.accept(getName, ())
                        lExp.add(classConstructor.exp_)
                        lExp.add(new Epreop(new Address, new Evar(objName))) // TODO: address shouldnt be added for objects on heap, fix when "new" is added
                        val funcCall = new ExprS(new SexprTwo(new Efunkpar(new Evar(className ++ "_init"), lExp)))
                        val declarator = new NoPointer(new Name(objName))
                        val initD = new OnlyDecl(declarator)
                        val listInitD = new ListInit_declarator
                        listInitD.add(initD)

                        val newDec = new Declarators(copyAst(decls.listdeclaration_specifier_), listInitD, new ListExtra_specifier)
                        val nD = newDec.accept(this, ())
                        val newDecS = new DecS(nD)

                        stmts.add(newDecS)
                        stmts.add(funcCall)
                      case _ => stmts.add(x.accept(this, ()))
                    }
                  }
                case _ => stmts.add(x.accept(this, ()))
              }
            case _ => stmts.add(x.accept(this, ()))
          }
        }
        copyLocationInformation(stm, new ScompTwo(stmts))
      }
    }
}
