------------------------------------------------------------------------
--- This library provides some useful operations to write programs
--- that generate AbstractCurry programs in a more compact and readable way.
---
--- @version September 2024
------------------------------------------------------------------------

module AbstractCurry.Build where

import AbstractCurry.Types

infixr 9 ~>

------------------------------------------------------------------------
-- Goodies to construct programs

--- Constructs a simple `CurryProg` without type classes and instances.
simpleCurryProg :: String -> [String] -> [CTypeDecl] -> [CFuncDecl] -> [COpDecl]
                -> CurryProg
simpleCurryProg name imps types funcs ops =
  CurryProg name imps Nothing [] [] types funcs ops

------------------------------------------------------------------------
-- Goodies to construct entities related to type classes

--- Constructs a simple class instance for a given type and
--- without a class constraint. Thus, the instance definition has the form
---
---     instance c ty where { ...;fundecl;... }
simpleInstanceDecl :: QName -> CTypeExpr -> [CFuncDecl] -> CInstanceDecl
simpleInstanceDecl qc te fdecls = CInstance qc (CContext []) [te] fdecls

------------------------------------------------------------------------
-- Goodies to construct type declarations

--- Constructs a simple constructor declaration without quantified
--- type variables and type class constraints.
simpleCCons :: QName -> CVisibility -> [CTypeExpr] -> CConsDecl
simpleCCons = CCons

------------------------------------------------------------------------
-- Goodies to construct type expressions

--- A type application of a qualified type constructor name to a list of
--- argument types.
applyTC :: QName -> [CTypeExpr] -> CTypeExpr
applyTC f es = foldl CTApply (CTCons f) es 

--- A function type.
(~>) :: CTypeExpr -> CTypeExpr -> CTypeExpr
t1 ~> t2 = CFuncType t1 t2

--- A base type.
baseType :: QName -> CTypeExpr
baseType t = CTCons t

--- Constructs a list type from an element type.
listType :: CTypeExpr -> CTypeExpr
listType a = CTApply (CTCons (pre "[]")) a

--- Constructs a tuple type from list of component types.
tupleType :: [CTypeExpr] -> CTypeExpr
tupleType ts
 | l==0 = baseType (pre "()")
 | l==1 = head ts
 | otherwise = foldl CTApply
                     (CTCons (pre ('(' : take (l-1) (repeat ',') ++ ")")))
                     ts
 where l = length ts

--- Constructs an IO type from a type.
ioType :: CTypeExpr -> CTypeExpr
ioType a = CTApply (CTCons (pre "IO")) a

--- Constructs a Maybe type from element type.
maybeType :: CTypeExpr -> CTypeExpr
maybeType a = CTApply (CTCons (pre "Maybe")) a

--- The type expression of the String type.
stringType :: CTypeExpr
stringType = baseType (pre "String")

--- The type expression of the Int type.
intType :: CTypeExpr
intType = baseType (pre "Int")

--- The type expression of the Float type.
floatType :: CTypeExpr
floatType = baseType (pre "Float")

--- The type expression of the Bool type.
boolType :: CTypeExpr
boolType = baseType (pre "Bool")

--- The type expression of the Char type.
charType :: CTypeExpr
charType = baseType (pre "Char")

--- The type expression of the unit type.
unitType :: CTypeExpr
unitType = baseType (pre "()")

--- The type expression of the Time.CalendarTime type.
dateType :: CTypeExpr
dateType = baseType ("Time", "CalendarTime")

--- A qualified type with empty class constraints.
emptyClassType :: CTypeExpr -> CQualTypeExpr
emptyClassType te = CQualType (CContext []) te

--- A qualified type with a single class constraint.
--- The arguments are the class name, the actual type parameter of the class,
--- and the type expression constrained by the class constraint.
singleClassType :: QName -> CTypeExpr -> CTypeExpr -> CQualTypeExpr
singleClassType qc clsarg te = CQualType (CContext [(qc,[clsarg])]) te

--- A class constraint with a single parameter.
--- The arguments are the class name and the type parameter of the class.
singleCConstraint :: QName -> CTypeExpr -> CConstraint
singleCConstraint qc clsarg = (qc,[clsarg])

------------------------------------------------------------------------
-- Goodies to construct function declarations

--- Constructs a function declaration from a given qualified function name,
--- arity, visibility, type expression and list of defining rules.
cfunc :: QName -> Int -> CVisibility -> CQualTypeExpr -> [CRule] -> CFuncDecl
cfunc = CFunc

--- Constructs a function declaration from a given comment,
--- qualified function name,
--- arity, visibility, type expression and list of defining rules.
cmtfunc :: String -> QName -> Int -> CVisibility -> CQualTypeExpr -> [CRule]
        -> CFuncDecl
cmtfunc = CmtFunc

-- Constructs a `CFunc` with simple (unqualified) type expression.
stFunc :: QName -> Int -> CVisibility -> CTypeExpr -> [CRule] -> CFuncDecl
stFunc name arity vis texp rs = cfunc name arity vis (emptyClassType texp) rs

-- Constructs a `CmtFunc` with simple (unqualified) type expression.
stCmtFunc :: String -> QName -> Int -> CVisibility -> CTypeExpr -> [CRule]
          -> CFuncDecl
stCmtFunc cm name arity vis texp rs =
  cmtfunc cm name arity vis (emptyClassType texp) rs

--- Constructs a simple rule with a pattern list and an
--- unconditional right-hand side.
simpleRule :: [CPattern] -> CExpr -> CRule
simpleRule pats rhs = CRule pats (CSimpleRhs rhs [])

--- Constructs a simple rule with a pattern list, an
--- unconditional right-hand side, and local declarations.
simpleRuleWithLocals :: [CPattern] -> CExpr -> [CLocalDecl] -> CRule
simpleRuleWithLocals pats rhs ldecls = CRule pats (CSimpleRhs rhs ldecls)

--- Constructs a rule with a possibly guarded right-hand side
--- and local declarations.
--- A simple right-hand side is constructed if there is only one
--- `True` condition.
guardedRule :: [CPattern] -> [(CExpr,CExpr)] -> [CLocalDecl] -> CRule
guardedRule pats gs ldecls
  | length gs == 1 && fst (head gs) == CSymbol (pre "True")
              = CRule pats (CSimpleRhs (snd (head gs)) ldecls)
  | otherwise = CRule pats (CGuardedRhs gs ldecls)

--- Constructs a guarded expression with the trivial guard.
noGuard :: CExpr -> (CExpr, CExpr)
noGuard e = (CSymbol (pre "True"), e)

--- Transforms an expression into a simple unconditional right-hand side.
simpleRhs :: CExpr -> CRhs
simpleRhs exp = CSimpleRhs exp []

------------------------------------------------------------------------
-- Goodies to construct expressions and patterns

--- An application of a qualified function name to a list of arguments.
applyF :: QName -> [CExpr] -> CExpr
applyF f es = foldl CApply (CSymbol f) es 

--- An application of an expression to a list of arguments.
applyE :: CExpr -> [CExpr] -> CExpr
applyE f args = foldl CApply f args

--- A constant, i.e., an application without arguments.
constF :: QName -> CExpr
constF f = applyF f []

--- An application of a variable to a list of arguments.
applyV :: CVarIName -> [CExpr] -> CExpr
applyV v es = foldl CApply (CVar v) es 

-- Applies the Just constructor to an AbstractCurry expression.
applyJust :: CExpr -> CExpr
applyJust a = applyF (pre "Just") [a]

-- Applies the maybe function to three AbstractCurry expressions.
applyMaybe :: CExpr -> CExpr -> CExpr -> CExpr
applyMaybe a1 a2 a3 = applyF (pre "maybe") [a1,a2,a3]

--- Constructs a tuple expression from list of component expressions.
tupleExpr :: [CExpr] -> CExpr
tupleExpr es | l==0 = constF (pre "()")
             | l==1 = head es
             | otherwise = applyF (pre ('(' : take (l-1) (repeat ',') ++ ")"))
                                  es
 where l = length es

--- Constructs an if-then-else expression.
ifThenElseExp :: CExpr -> CExpr -> CExpr -> CExpr
ifThenElseExp bexp texp eexp = applyF (pre "if_then_else") [bexp, texp, eexp]

--- Constructs a let declaration (with possibly empty local delcarations).
letExpr :: [CLocalDecl] -> CExpr -> CExpr
letExpr locals cexp = if null locals then cexp else CLetDecl locals cexp

--- Constructs a typed expression from an expression and a simple type.
simpleTyped :: CExpr -> CTypeExpr -> CExpr
simpleTyped exp texp = CTyped exp (emptyClassType texp)

--- Constructs a do expression. If the list of statements in the do expression
--- contains a single expression, the do expression is transformed into
--- a simple expression.
doExpr :: [CStatement] -> CExpr
doExpr stats = case stats of [CSExpr exp] -> exp
                             _            -> CDoExpr stats

--- Constructs from a pattern and an expression a branch for a case expression.
cBranch :: CPattern -> CExpr -> (CPattern, CRhs)
cBranch pattern exp = (pattern, CSimpleRhs exp [])

--- Constructs a tuple pattern from list of component patterns.
tuplePattern :: [CPattern] -> CPattern
tuplePattern ps
  | l==0 = CPComb (pre "()") []
  | l==1 = head ps
  | otherwise = CPComb (pre ('(' : take (l-1) (repeat ',') ++ ")")) ps
 where l = length ps

--- Constructs, for given n, a list of n PVars starting from 0.
pVars :: Int -> [CPattern]
pVars n = [CPVar (i,"x"++show i) | i<-[0..n-1]] 

--- Converts an integer into an AbstractCurry expression.
pInt :: Int -> CPattern
pInt x = CPLit (CIntc x)

--- Converts a float into an AbstractCurry expression.
pFloat :: Float -> CPattern
pFloat x = CPLit (CFloatc x)

--- Converts a character into a pattern.
pChar :: Char -> CPattern
pChar x = CPLit (CCharc x)

--- Constructs an empty list pattern.
pNil :: CPattern
pNil = CPComb (pre "[]") []

--- Constructs a list pattern from list of component patterns.
listPattern :: [CPattern] -> CPattern
listPattern []     = pNil
listPattern (p:ps) = CPComb (pre ":") [p, listPattern ps]

--- Converts a string into a pattern representing this string.
stringPattern :: String -> CPattern
stringPattern = CPLit . CStringc

--- Converts a list of AbstractCurry expressions into an
--- AbstractCurry representation of this list.
list2ac :: [CExpr] -> CExpr
list2ac []     = constF (pre "[]")
list2ac (c:cs) = applyF (pre ":") [c, list2ac cs]

--- Converts an integer into an AbstractCurry expression.
cInt :: Int -> CExpr
cInt x = CLit (CIntc x)

--- Converts a float into an AbstractCurry expression.
cFloat :: Float -> CExpr
cFloat x = CLit (CFloatc x)

--- Converts a character into an AbstractCurry expression.
cChar :: Char -> CExpr
cChar x = CLit (CCharc x)

--- Converts a string into an AbstractCurry represention of this string.  
string2ac :: String -> CExpr
string2ac s = CLit (CStringc s)

--- Converts an index i into a variable named xi.
toVar :: Int -> CExpr
toVar i = CVar (1,"x"++show i)

--- Converts a string into a variable with index 1.
cvar :: String -> CExpr
cvar s = CVar (1,s)

--- Converts a string into a pattern variable with index 1.
cpvar :: String -> CPattern
cpvar s = CPVar (1,s)

--- Converts a string into a type variable with index 1.
ctvar :: String -> CTypeExpr
ctvar s = CTVar (1,s)

------------------------------------------------------------------------
