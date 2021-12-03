with Minerva.Names;

private package Minerva.Operators is

   type Minerva_Operator is
     (Op_None,
      Op_Abs, Op_Not, Op_Negate,
      Op_Exponent,
      Op_Multiply, Op_Divide,
      Op_Add, Op_Subtract,
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
