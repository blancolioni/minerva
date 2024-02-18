with Minerva.Names;

private package Minerva.Operators is

   type Minerva_Operator is
     (Op_None,
      Op_Abs, Op_Not, Op_Neg,
      Op_Exp,
      Op_Mul, Op_Div, Op_Mod,
      Op_Add, Op_Sub,
      Op_LE, Op_LT, Op_GE, Op_GT,
      Op_EQ, Op_NE,
      Op_And, Op_Or);

   function Get_Name
     (Operator : Minerva_Operator)
      return Minerva.Names.Minerva_Name;

   function Get_Operator
     (Name : Minerva.Names.Minerva_Name)
      return Minerva_Operator;

end Minerva.Operators;
