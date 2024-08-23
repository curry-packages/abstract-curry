-- ---------------------------------------------------------------------------
--- This library contains a definition for representing Curry programs
--- in Curry.
---
--- Note this defines a slightly new format for AbstractCurry
--- in comparison to the first proposal of 2003.
---
--- Assumption: an abstract Curry program is stored in file with
--- extension .acy
---
--- @author Michael Hanus, Bjoern Peemoeller, Finn Teegen
--- @version August 2024
-- ---------------------------------------------------------------------------

module AbstractCurry.Types where

-- ---------------------------------------------------------------------------
-- Definition of data types for representing abstract Curry programs:
-- ---------------------------------------------------------------------------

--- Current version of AbstractCurry
version :: String
version = "AbstractCurry 4.0"

--- A module name.
type MName = String

--- The data type for representing qualified names.
--- In AbstractCurry all names are qualified to avoid name clashes.
--- The first component is the module name and the second component the
--- unqualified name as it occurs in the source program.
--- An exception are locally defined names where the module name is
--- the empty string (to avoid name clashes with a globally defined name).
type QName = (MName, String)

--- Data type to specify the visibility of various entities.
data CVisibility
  = Public    -- exported entity
  | Private   -- private entity
  deriving (Eq, Show)
  

--- Data type for representing a Curry module in the intermediate form.
--- A value of this data type has the form
---
---     (CurryProg modname imports dfltdecl clsdecls instdecls typedecls
---                funcdecls opdecls)
---
--- where modname: name of this module,
---       imports: list of modules names that are imported,
---       dfltdecl: optional default declaration
---       clsdecls:  Class declarations
---       instdecls: Instance declarations
---       typedecls: Type declarations
---       functions: Function declarations
---       opdecls: Operator precedence declarations
data CurryProg = CurryProg MName [MName] (Maybe CDefaultDecl) [CClassDecl]
                           [CInstanceDecl] [CTypeDecl] [CFuncDecl] [COpDecl]
  deriving (Eq, Show)

--- Data type for representing default declarations.
data CDefaultDecl = CDefaultDecl [CTypeExpr]
  deriving (Eq, Show)

--- Data type for representing classes declarations.
---
--- A type class definition of the form
---
---     class cx => c a1 ... an | ..., lhsAs -> rhsAs, ... where { ...;f :: t;... }
---
--- , where each 'lhsAs' and 'rhsAs' is a selection of the variables 'a1 ... an'
--- and 'lhsAs -> rhsAs' is a functional dependency,
--- is represented by the Curry term
---
---     (CClass c v cx tvs funDeps [...(CFunc f ar v t [...,CRule r,...])...])
---
--- where 'tvs' is the list of indices of the type variables 'a1' to 'an',
--- 'funDeps' is a list of the form '[...,(lhsTvs,rhsTvs),...]', with 'lhsTvs'
--- and 'rhsTvs' being lists of indices in 'tvs' representing 'lhsAs' and 'rhsAs'
--- respectively, and 'v' is the visibility of the type class resp. method.
--- Note: Type class declarations with none or more than one type variable are
---       only allowed with the 'MultiParamTypeClasses' language extension.
---       Functional dependencies are only allowed with the
---       'FunctionalDependencies' language extension.
---       The type variable indices are unique inside each class
---       declaration and are usually numbered from 0.
---       The methods' types share the type class' type variable index
---       as the class variable has to occur in a method's type signature.
---       The list of rules for a method's declaration may be empty if
---       no default implementation is provided. The arity 'ar' is
---       determined by a given default implementation or 0.
---       Regardless of whether typed or untyped abstract curry is generated,
---       the methods' declarations are always typed.
data CClassDecl = CClass QName CVisibility CContext [CTVarIName] [CFunDep] [CFuncDecl]
  deriving (Eq, Show)

--- Data type for representing instance declarations.
---
--- An instance definition of the form
---
---     instance cx => c ty1 ... tyn where { ...;fundecl;... }
---
--- is represented by the Curry term
---
---     (CInstance c cx [ty1, ..., tyn] [...fundecl...])
---
--- Note: Instance declarations with none or more than one instance type are 
---       only allowed with the 'MultiParamTypeClasses' language extension.
---       The type variable indices are unique inside each instance
---       declaration and are usually numbered from 0.
---       The methods' types use the instance's type variable indices
---       (if typed abstract curry is generated).
data CInstanceDecl = CInstance QName CContext [CTypeExpr] [CFuncDecl]
  deriving (Eq, Show)

--- Data type for representing definitions of algebraic data types
--- and type synonyms.
---
--- A data type definition of the form
---
---     data t x1...xn = ...| c t1....tkc |...
---       deriving (d1,...,dp)
---
--- is represented by the Curry term
---
---     (CType t v [i1,...,in]
---            [...(CCons c v [t1,...,tkc])...] [d1,...,dp]))
---
--- where each `ij` is the index of the type variable `xj` and 'v' is the
--- visibility of the type resp. constructor.
---
--- Note: the type variable indices are unique inside each type declaration
---       and are usually numbered from 0
---
--- Thus, a data type declaration consists of the name of the data type,
--- a list of type parameters and a list of constructor declarations.
data CTypeDecl
  = CType    QName CVisibility [CTVarIName] [CConsDecl] [QName]
  | CTypeSyn QName CVisibility [CTVarIName] CTypeExpr
  | CNewType QName CVisibility [CTVarIName] CConsDecl   [QName]
  deriving (Eq, Show)

--- The type for representing type variables.
--- They are represented by (i,n) where i is a type variable index
--- which is unique inside a function and n is a name (if possible,
--- the name written in the source program).
type CTVarIName = (Int, String)

--- A constructor declaration consists of the name of the constructor
--- and a list of the argument types of the constructor.
--- The arity equals the number of types.
data CConsDecl
  = CCons   QName CVisibility [CTypeExpr]
  | CRecord QName CVisibility [CFieldDecl]
  deriving (Eq, Show)

--- A record field declaration consists of the name of the
--- the label, the visibility and its corresponding type.
data CFieldDecl = CField QName CVisibility CTypeExpr
  deriving (Eq, Show)

--- Class constraint.
type CConstraint = (QName, [CTypeExpr])

--- Context.
data CContext = CContext [CConstraint]
  deriving (Eq, Show)

--- The type for representing a functional dependency of a type class.
type CFunDep = ([CTVarIName], [CTVarIName])

--- Type expression.
--- A type expression is either a type variable, a function type,
--- or a type constructor application.
---
--- Note: the names of the predefined type constructors are
---       "Int", "Float", "Bool", "Char", "IO",
---       "()" (unit type), "(,...,)" (tuple types), "[]" (list type)
data CTypeExpr
  = CTVar CTVarIName               -- type variable
  | CFuncType CTypeExpr CTypeExpr  -- function type t1->t2
  | CTCons QName                   -- type constructor
  | CTApply CTypeExpr CTypeExpr    -- type application
  deriving (Eq, Show)

--- Qualified type expression.
data CQualTypeExpr = CQualType CContext CTypeExpr
  deriving (Eq, Show)

--- Labeled record fields
type CField a = (QName, a)

--- Data type for operator declarations.
--- An operator declaration "fix p n" in Curry corresponds to the
--- AbstractCurry term (COp n fix p).
data COpDecl = COp QName CFixity Int
  deriving (Eq, Show)

--- Data type for operator associativity
data CFixity
  = CInfixOp   -- non-associative infix operator
  | CInfixlOp  -- left-associative infix operator
  | CInfixrOp  -- right-associative infix operator
  deriving (Eq, Show)

--- Function arity
type Arity = Int

--- Data type for representing function declarations.
---
--- A function declaration in AbstractCurry is a term of the form
---
---     (CFunc name arity visibility type (CRules eval [CRule rule1,...,rulek]))
---
--- and represents the function `name` defined by the rules
--- `rule1,...,rulek`.
---
--- Note: the variable indices are unique inside each rule
---
--- Thus, a function declaration consists of the name, arity, type, and
--- a list of rules. The type is the function's type inferred by the
--- type checker. However, if an AbstractCurry program is read with
--- the operation `AbstractCurry.Files.readUntypedCurry`, the type
--- is either the type signature provided by the programmer or
--- the expression `(CTCons ("Prelude","untyped")`
--- if the programmer has not provided an explicit type signature.
---
--- A function declaration with the constructor `CmtFunc`
--- is similarly to `CFunc` but has a comment
--- as an additional first argument. This comment could be used
--- by pretty printers that generate a readable Curry program
--- containing documentation comments.
data CFuncDecl
  = CFunc          QName Arity CVisibility CQualTypeExpr [CRule]
  | CmtFunc String QName Arity CVisibility CQualTypeExpr [CRule]
  deriving (Eq, Show)

--- The general form of a function rule. It consists of a list of patterns
--- (left-hand side) and the right-hand side for these patterns.
data CRule = CRule [CPattern] CRhs
  deriving (Eq, Show)

--- Right-hand-side of a 'CRule' or a `case` expression.
--- It is either a simple unconditional right-hand side or
--- a list of guards with their corresponding right-hand sides, and
--- a list of local declarations.
data CRhs
  = CSimpleRhs  CExpr            [CLocalDecl] -- expr where decls
  | CGuardedRhs [(CExpr, CExpr)] [CLocalDecl] -- | cond = expr where decls
  deriving (Eq, Show)

--- Data type for representing local (let/where) declarations
data CLocalDecl
  = CLocalFunc CFuncDecl     -- local function declaration
  | CLocalPat  CPattern CRhs -- local pattern declaration
  | CLocalVars [CVarIName]   -- local free variable declaration
  deriving (Eq, Show)

--- Data types for representing object variables.
--- Object variables occurring in expressions are represented by (Var i)
--- where i is a variable index.
type CVarIName = (Int,String)

--- Data type for representing pattern expressions.
data CPattern
  = CPVar      CVarIName               -- pattern variable (unique index / name)
  | CPLit      CLiteral                -- literal (Integer/Float/Char constant)
  | CPComb     QName [CPattern]        -- application (m.c e1 ... en) of n-ary
                                       -- constructor m.c (CPComb (m,c) [e1,...,en])
  | CPAs       CVarIName CPattern      -- as-pattern (extended Curry)
  | CPFuncComb QName [CPattern]        -- function pattern (extended Curry)
  | CPLazy     CPattern                -- lazy pattern (extended Curry)
  | CPRecord   QName [CField CPattern] -- record pattern (extended Curry)
  deriving (Eq, Show)

--- Data type for representing Curry expressions.
data CExpr
 = CVar       CVarIName                    -- variable (unique index / name)
 | CLit       CLiteral                     -- literal (Int/Float/Char constant)
 | CSymbol    QName                        -- a defined symbol (qualified name)
 | CApply     CExpr CExpr                        -- application (e1 e2)
 | CLambda    [CPattern] CExpr                   -- lambda abstraction
 | CLetDecl   [CLocalDecl] CExpr                 -- local let declarations
 | CDoExpr    [CStatement]                       -- do expression
 | CListComp  CExpr [CStatement]                 -- list comprehension
 | CCase      CCaseType CExpr [(CPattern, CRhs)] -- case expression
 | CTyped     CExpr CQualTypeExpr                -- typed expression
 | CRecConstr QName [CField CExpr]         -- record construction
 | CRecUpdate CExpr [CField CExpr]         -- record update
 deriving (Eq, Show)

--- Data type for representing literals occurring in an expression.
--- It is either an integer, a float, or a character constant.
data CLiteral
  = CIntc   Int
  | CFloatc Float
  | CCharc  Char
  | CStringc String
 deriving (Eq, Show)

--- Data type for representing statements in do expressions and
--- list comprehensions.
data CStatement
  = CSExpr CExpr         -- an expression (I/O action or boolean)
  | CSPat CPattern CExpr -- a pattern definition
  | CSLet [CLocalDecl]   -- a local let declaration
  deriving (Eq, Show)

--- Type of case expressions
data CCaseType
  = CRigid -- rigid case expression
  | CFlex  -- flexible case expression
  deriving (Eq, Show)

---------------------------------------------------------------------------
--- The name of the standard prelude.
preludeName :: String
preludeName = "Prelude"

--- Converts a string into a qualified name of the Prelude.
pre :: String -> QName
pre f = (preludeName, f)

---------------------------------------------------------------------------
