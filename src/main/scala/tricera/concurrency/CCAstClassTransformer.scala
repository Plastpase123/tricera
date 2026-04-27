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
    classDecs  : ListBuffer[Struct_dec],
    classFuncs : ListBuffer[Afunc])

  private case class ClassDefCollectionResult(
    classDefs : Map[String, ClassDefInfo]
  )

  private val printer = new PrettyPrinterNonStatic()

  def transform(program: Program): Program = {
    //println("=== ORIGINAL PROGRAM === ")
    //println(printer print program)

    val collectionResult = collectClassDefs(program)
    if (collectionResult.classDefs.isEmpty)
      return program
    val transformer = new ClassTransformer(collectionResult)
  val p = program.accept(transformer, (null, null))

  //println("=== TRANSFORMED PROGRAM === ")
  //println(printer print p)
  return p
  }

  private def collectClassDefs(program : Program) : ClassDefCollectionResult = {
    val classDefBuffer = new MHashMap[String, ClassDefInfo]
    val collector = new ClassDefCollector(classDefBuffer)
    program.accept(collector, (null,null))

    ClassDefCollectionResult(classDefBuffer.toMap)
  }

  private def getStructDec(sd : Struct_dec) : Struct_dec = {
    sd match {
      case as : AccSpec => getStructDec(as.struct_dec_)
      case _ => sd
    }
  }

  // Visitor to qualify nested class definitions.
  // Can be done in tri-pp, but it complains when compiling.
  private class RenameClassVisitor
  extends CCAstCopyWithLocation[MHashMap[String,String]] {
    val classStack = new Stack[String]
    override def visit(dec : Tag, classMap : MHashMap[String, String]): Struct_or_union_spec = {
      dec.struct_or_union_ match {
        case _ : Class =>
          val listStructDec = new ListStruct_dec
          val className = dec.cident_
          val newName = classMap.getOrElse(className, className)
          dec.liststruct_dec_.forEach(s => listStructDec.add(s.accept(this, classMap)))
          new Tag(dec.struct_or_union_, newName, listStructDec)
        case _ => dec
      }
    }

    override def visit(dec : TagType, classMap : MHashMap[String, String]): Struct_or_union_spec = {
      dec.struct_or_union_ match {
        case _ : Class =>
          val className = dec.cident_
          val newName = classMap.getOrElse(className, className)
          new TagType(dec.struct_or_union_, newName)
        case _ => dec
      }
    }
  }
  private class IsClassVisitor() extends AbstractVisitor[Boolean, Unit] {

    def structDecIsClass(s : Struct_dec) : Boolean = {
      getStructDec(s) match {
        case structen : Structen =>
          val types = for (t <- structen.listspec_qual_.asScala if t.isInstanceOf[TypeSpec]) yield t.asInstanceOf[TypeSpec]
          val structs = for (t <- types if t.type_specifier_.accept(this, ())) yield t
          if (structs.isEmpty) { false }
          else { true }

        case noDec : StructenNoDec =>
          val types = for (t <- noDec.listspec_qual_.asScala if t.isInstanceOf[TypeSpec]) yield t.asInstanceOf[TypeSpec]
          val structs = for (t <- types if t.type_specifier_.accept(this, ())) yield t
          if (structs.isEmpty) { false }
          else { true }
        case _ => false
      }
    }

    override def visit(s : Tstruct, arg : Unit) = { s.struct_or_union_spec_.accept(this, ()) }
    override def visitDefault(t : Type_specifier, arg : Unit) = { false }

    override def visit(s : Tag, arg : Unit) = { s.struct_or_union_.accept(this, ()) }
    override def visit(s : Unique, arg : Unit) = { s.struct_or_union_.accept(this, ()) }
    override def visit(s : TagType, arg : Unit) = { false }
    override def visitDefault(s : Struct_or_union_spec, arg : Unit) = { false }

    override def visit(s : Class, arg : Unit) = { true }
    override def visitDefault(s : Struct_or_union, arg : Unit) = { false }
  }

  private class ClassDefCollector(
    val classDefBuffer : MHashMap[String, ClassDefInfo]
  ) extends ComposVisitor[(ListBuffer[Struct_dec], ListBuffer[Afunc])] {
    private val getName = new CCAstGetNameVistor
    private val rename  = new CCAstRenameInDeclarationVistor
    private val classMap = new MHashMap[String, String]
    private val renameClass = new RenameClassVisitor
    private val isClass = new IsClassVisitor
    private val classStack = new Stack[String]

    private def qualified(name: String): String =
      (classStack.reverse :+ name).mkString("::")

    override def visit(fun : ClassConstr, decls : (ListBuffer[Struct_dec], ListBuffer[Afunc])) : ClassConstr = {
      fun.direct_declarator_ match {
        case fd : NewFuncDec =>
         // Get fully qualified name from classMap for class that is being processed (i.e. top of stack)
         val addConstr = (_ : Any) => classMap(classStack.head) ++ "::constr"
          val renamed = fd.direct_declarator_.accept(rename, addConstr(_))
          val funcDec = new NoPointer(new NewFuncDec(renamed, fd.parameter_type_))
          val listDeclarationSpecifier = new ListDeclaration_specifier
          listDeclarationSpecifier.add(new Type(new Tvoid))
          val newAfunc = new Afunc(new NewFunc(listDeclarationSpecifier, funcDec, fun.compound_stm_))
          decls._2 += newAfunc
          fun
        case fd : OldFuncDec =>
          val addConstr = (_ : Any) => classMap(classStack.head) ++ "::constr"
          val renamed = fd.direct_declarator_.accept(rename, addConstr(_))
          val funcDec = new NoPointer(new OldFuncDec(renamed))
          val listDeclarationSpecifier = new ListDeclaration_specifier
          listDeclarationSpecifier.add(new Type(new Tvoid))
          val newAfunc = new Afunc(new NewFunc(listDeclarationSpecifier, funcDec, fun.compound_stm_))
          decls._2 += newAfunc
          fun
        case _ => fun
      }
    }


    def isFuncOrClassConsDec(dir : Direct_declarator): Boolean = {
      dir.isInstanceOf[NewFuncDec] || dir.isInstanceOf[OldFuncDec] || dir.isInstanceOf[ClassCons]
    }

    def isValidStructField(decl : Decl): Boolean = decl.declarator_ match {
      case p : BeginPointer => !isFuncOrClassConsDec(p.direct_declarator_)
      case np : NoPointer => !isFuncOrClassConsDec(np.direct_declarator_)
      case _ => false // Should never happen
    }

    def getMethodAsAfunc(className : String, s : Structen, f : ClassFunc): Afunc = {
      val listDeclarationSpecifier = new ListDeclaration_specifier
      val types = s.listspec_qual_.asScala.collect{ case ts : TypeSpec => new Type(ts.type_specifier_) }
      val quals = s.listspec_qual_.asScala.collect{ case qs : QualSpec => new SpecProp(qs.type_qualifier_) }

      quals.foreach(listDeclarationSpecifier.add)
      types.foreach(listDeclarationSpecifier.add)

      // We store member functions qualified with the fully qualified name of the class
      val renamedDec = {
        val qualifyName = (s : String) => classMap(className) ++ "::" ++ s
        f.declarator_ match {
          case bp : BeginPointer =>
            bp.direct_declarator_ match {
              case fd : NewFuncDec =>
                val renamed = fd.direct_declarator_.accept(rename, qualifyName)
                new BeginPointer(bp.pointer_, new NewFuncDec(renamed, fd.parameter_type_))
              case fd : OldFuncDec =>
                val renamed = fd.direct_declarator_.accept(rename, qualifyName)
                new BeginPointer(bp.pointer_, new OldFuncDec(renamed))
              case _ => f.declarator_
            }
          case np : NoPointer =>
            np.direct_declarator_ match {
              case fd : NewFuncDec =>
                val renamed = fd.direct_declarator_.accept(rename, qualifyName)
                new NoPointer(new NewFuncDec(renamed, fd.parameter_type_))
              case fd : OldFuncDec =>
                val renamed = fd.direct_declarator_.accept(rename, qualifyName)
                new NoPointer(new OldFuncDec(renamed))
              case _ => f.declarator_
            }
        }
      }
      new Afunc(new NewFunc(listDeclarationSpecifier, renamedDec, f.compound_stm_))
    }

    def collectClassMetadata(className : String, s : Structen, decls : (ListBuffer[Struct_dec], ListBuffer[Afunc])): Unit = {
      if (isClass.structDecIsClass(s)) return
      val fields = s.liststruct_declarator_.asScala.collect {
        case decl : Decl if isValidStructField(decl) => decl
      }
      val functions = s.liststruct_declarator_.asScala.collect {
        case f : ClassFunc => getMethodAsAfunc(className, s, f)
      }
      if (!(fields.isEmpty)) {
        val listStructDeclarator = new ListStruct_declarator
        fields.foreach(listStructDeclarator.add)
        val newStructen = new Structen(s.listspec_qual_, listStructDeclarator, s.break_)
        decls._1 += newStructen
      }
      if (!(functions.isEmpty)) {
        functions.foreach(f => decls._2 += f)
      }
    }


    override def visit(c : Tag, arg : (ListBuffer[Struct_dec], ListBuffer[Afunc])) : Tag = c.struct_or_union_ match {
      case _ : Class =>
        val className = c.cident_
        val qualifiedName = { // If class definition is nested, get correctly qualified name
          if (classStack.isEmpty) {
            classMap.clear()
            className
          } else qualified(className)
        }
        classStack.push(className)
        classMap.put(className, qualifiedName)

        val decls = (new ListBuffer[Struct_dec], new ListBuffer[Afunc])
        c.liststruct_dec_.forEach(s => s.accept(this, decls))


        val listStructDec = new ListStruct_dec
        val structDecs = c.liststruct_dec_.asScala.map(dec => getStructDec(dec))
        val structens = structDecs.collect { case s : Structen => s }
        val specialFuncs = structDecs.collect { case constr : ClassConstr => constr }
        structens.foreach(s => collectClassMetadata(className, s, decls))
        //specialFuncs.foreach(f => f.accept(this, decls))

        val classDecs = decls._1
        val classFuncs = decls._2
        val collectedClassInfo = ClassDefInfo(classDecs, classFuncs)
        classStack.pop()
        classDefBuffer.put(qualifiedName, collectedClassInfo)
        c
      case _ => c
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
      val isClass = new IsClassVisitor
      val classObjBuffer = new MHashMap[String, String]


      // Adds a class object pointer to the parameters of a member function
      // of the form "className *this"
      private def createStructParam(className : String) : Parameter_declaration = {
        val listDeclarationSpecifier = new ListDeclaration_specifier
        val t = new Type(new Tstruct(new TagType(new Struct, className)))
        val param = new BeginPointer(new Point, new Name("this"))

        listDeclarationSpecifier.addFirst(t)

        new TypeAndParam(listDeclarationSpecifier, param)
      }

      // Transforms a member function into a global function definition.
      private def createClassAfunc(fun : Afunc, className : String) : External_declaration = {
        val funcDef = fun.function_def_
        val funcDec = funcDef.accept(getFunctionDeclaration, ())

       val (listDecSpecifier, funcName) = (funcDec._1, fun.accept(getName, ()))

        val initDec = funcDec._2
        val dec = initDec.accept(getDeclarator, ())
        val params = dec.accept(getParameters, ())
        val body = funcDef.accept(getFuncBody, ())
        val classParam = createStructParam(className)
        params.addFirst(classParam)
        new Afunc(new NewFunc(listDecSpecifier, new NoPointer(new NewFuncDec(new Name(funcName), new AllSpec(params))), body))

      }

      private def createGlobalStruct(className : String, structDecs : ListBuffer[Struct_dec]) : External_declaration = {
        val listDeclarationSpecifier = new ListDeclaration_specifier
        val listExtraSpecifier = new ListExtra_specifier
        val listStructDec = new ListStruct_dec

        structDecs.foreach(d => listStructDec.add(d))

        val structType = new Type(new Tstruct(new Tag(new Struct, className, listStructDec)))
        listDeclarationSpecifier.add(structType)

        new Global(new NoDeclarator(listDeclarationSpecifier, listExtraSpecifier))
      }

      // Transforms the existing AST nodes and adds member functions as global function definitions
      override def visit(p : Progr, arg : Any) : Program = {
        val originalProgDecs = p.listexternal_declaration_
        val extDeclarations = new ListExternal_declaration
        val defBuffer = collectedDefs.classDefs

        for (className <- defBuffer.keys) {
          val c = defBuffer(className)
          extDeclarations.add(createGlobalStruct(className, c.classDecs))
          for (fun <- c.classFuncs) {
            extDeclarations.addLast(createClassAfunc(fun, className).accept(this, ()))
          }
        }

        for (x <- originalProgDecs.asScala) {
          val result = x.accept(this, arg)
          if (result != null) extDeclarations.addLast(result) // Use something else instead of null if possible
        }
        val newProg = new Progr(extDeclarations)
        copyLocationInformation(p, newProg)
      }

      // Replaces class keywords with struct
      override def visit(ctype : Class, arg : Any) : Struct_or_union = {
        copyLocationInformation(ctype, new Struct)
      }

      // Checks whether a declaration has a definition
      private def hasInlineClassDef(d : Dec) : Boolean = {
        val listDeclarationSpecifier = {
          d match {
            case nd : NoDeclarator => nd.listdeclaration_specifier_
            case decls : Declarators => decls.listdeclaration_specifier_
            case _ => return false
          }
        }
        listDeclarationSpecifier.asScala.exists {
          case t : Type => t.type_specifier_ match {
            case ts : Tstruct => ts.struct_or_union_spec_ match {
              case tag : Tag => tag.struct_or_union_.isInstanceOf[Class]
              case _         => false
            }
            case _ => false
          }
          case _ => false
        }
      }

      override def visit(g : Global, arg : Any) : External_declaration = {
        g.dec_ match {
          case nd : NoDeclarator if hasInlineClassDef(nd) =>
            null // Very ugly hack

          case decls : Declarators if hasInlineClassDef(decls) =>
            val listDeclarationSpecifier = new ListDeclaration_specifier
            decls.listdeclaration_specifier_.forEach { ds =>
              ds match {
                case t : Type => t.type_specifier_ match {
                  case ts : Tstruct => ts.struct_or_union_spec_ match {
                    case tag : Tag if tag.struct_or_union_.isInstanceOf[Class] =>
                      // Replace the inline definition with a TagType forward reference.
                      listDeclarationSpecifier.add(new Type(new Tstruct(new TagType(new Struct, tag.cident_))))
                    case _ => listDeclarationSpecifier.add(ds.accept(this, arg))
                  }
                  case _ => listDeclarationSpecifier.add(ds.accept(this, arg))
                }
                case _ => listDeclarationSpecifier.add(ds.accept(this, arg))
              }
            }
            val listInitDeclarator = new ListInit_declarator
            decls.listinit_declarator_.forEach(id => listInitDeclarator.add(id.accept(this, arg)))
            val listExtraSpecifier = new ListExtra_specifier
            decls.listextra_specifier_.forEach(e => listExtraSpecifier.add(e.accept(this, arg)))
            copyLocationInformation(g, new Global(new Declarators(listDeclarationSpecifier, listInitDeclarator, listExtraSpecifier)))

          case _ =>
            copyLocationInformation(g, new Global(g.dec_.accept(this, arg)))
        }
      }


      override def visit(d : DecS, arg : Any) : DecS = {
        d.dec_ match {
          case nd : NoDeclarator if hasInlineClassDef(nd) =>
            null

          case decls : Declarators if hasInlineClassDef(decls) =>
            val listDeclarationSpecifier = new ListDeclaration_specifier
            decls.listdeclaration_specifier_.forEach { ds =>
              ds match {
                case t : Type => t.type_specifier_ match {
                  case ts : Tstruct => ts.struct_or_union_spec_ match {
                    case tag : Tag if tag.struct_or_union_.isInstanceOf[Class] =>
                      // Replace inline definition with just a TagType
                      listDeclarationSpecifier.add(new Type(new Tstruct(new TagType(new Struct, tag.cident_))))
                    case _ => listDeclarationSpecifier.add(ds.accept(this, arg))
                  }
                  case _ => listDeclarationSpecifier.add(ds.accept(this, arg))
                }
                case _ => listDeclarationSpecifier.add(ds.accept(this, arg))
              }
            }
            val listInitDeclarator = new ListInit_declarator
            decls.listinit_declarator_.forEach(id => listInitDeclarator.add(id.accept(this, arg)))
            val listExtraSpecifier = new ListExtra_specifier
            decls.listextra_specifier_.forEach(e => listExtraSpecifier.add(e.accept(this, arg)))
            copyLocationInformation(d, new DecS(new Declarators(listDeclarationSpecifier, listInitDeclarator, listExtraSpecifier)))

          case _ =>
            copyLocationInformation(d, new DecS(d.dec_.accept(this, arg)))
        }
      }

      // Transforms a call to a member function via a pointer from "Exp->funcName(...)" to "funcName(Exp, ...)"
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
        params.addFirst(param)
        val newExp = new Efunkpar(new Evar(funcName), params)
        copyLocationInformation(original, newExp.accept(this, ()))
      }

      // Transforms a call to a member function via a select from "Exp.funcName(...)" to "funcName(&Exp, ...)"
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
        params.addFirst(param.accept(this, ()))
        val newExp = new Efunkpar(new Evar(funcName), params)
        copyLocationInformation(original, newExp)
      }

      // Finds calls to member functions of the form "Exp.funcName()" or "Exp->funcName()"
      // and transforms them as described in buildCall
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
      // and transforms them as described in buildCall
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


      private def classNameFromDecl(decls : Declarators): String = {
        val types = new ListBuffer[Type_specifier]
        decls.listdeclaration_specifier_.forEach(_.accept(getType, types))
        types.collectFirst {
          case tstruct : Tstruct =>
            tstruct.struct_or_union_spec_ match {
              case tagType : TagType if tagType.struct_or_union_.isInstanceOf[Class] =>
                tagType.cident_
              case _ : TagType =>
                throw new ClassTransformException(s"Call to class constructor has wrong class key")
              case _ =>
                throw new ClassTransformException("Call to class constructor must be of type TagType")
            }
        }.getOrElse(throw new ClassTransformException("Class constructor isn't declared with a struct type"))
      }

      private def buildConstructorDec(decls : Declarators, classCons : ClassCons): List[Stm] = {
        val className = classNameFromDecl(decls)
        val objName = classCons.accept(getName, ())

        val params = new ListExp
        params.add(classCons.exp_)
        params.addFirst(new Epreop(new Address, new Evar(objName)))
        val funcCall = new ExprS(new SexprTwo(
          new Efunkpar(new Evar(className ++ "::constr"), params)))

        val listInitDec = new ListInit_declarator
        listInitDec.add(new OnlyDecl(new NoPointer(new Name(objName))))
        val newDec = new Declarators(
          copyAst(decls.listdeclaration_specifier_), listInitDec, new ListExtra_specifier)

        List(new DecS(newDec.accept(this, ())), funcCall)
      }

      private def expandStmt(stmt : Stm): List[Stm] = stmt match {
        case decS : DecS => decS.dec_ match {
          case decls : Declarators =>
            decls.listinit_declarator_.asScala.toList.flatMap { init =>
              val dirDec = init.accept(getDeclarator, ()) match {
                case d : BeginPointer => d.direct_declarator_
                case d : NoPointer => d.direct_declarator_
              }
              dirDec match {
                case classCons : ClassCons => buildConstructorDec(decls, classCons)
                case _ => List(stmt.accept(this, ()))
              }
            }
          case _ => List(stmt.accept(this, ()))
        }
        case _ => List(stmt.accept(this, ()))
      }

      override def visit(scomp : ScompTwo, arg : Any): ScompTwo = {
        val stmts = new ListStm
        scomp.liststm_.asScala.flatMap(expandStmt).foreach(stmts.add)
        copyLocationInformation(scomp, new ScompTwo(stmts))

      }
    }
}
