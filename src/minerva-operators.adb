with WL.String_Maps;

package body Minerva.Operators is

   package Op_Maps is new WL.String_Maps (Minerva_Operator);

   Name_Op_Map : Op_Maps.Map;
   Op_Name_Map : array (Minerva_Operator) of Minerva.Names.Minerva_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Operator : Minerva_Operator)
      return Minerva.Names.Minerva_Name
   is
   begin
      return Op_Name_Map (Operator);
   end Get_Name;

   ------------------
   -- Get_Operator --
   ------------------

   function Get_Operator
     (Name : Minerva.Names.Minerva_Name)
      return Minerva_Operator
   is
   begin
      return Name_Op_Map (Minerva.Names.Standard_Text (Name));
   end Get_Operator;

begin
   declare
      procedure Map (Op   : Minerva_Operator;
                     Name : String);

      ---------
      -- Map --
      ---------

      procedure Map (Op   : Minerva_Operator;
                     Name : String)
      is
      begin
         Name_Op_Map.Insert (Name, Op);
         Op_Name_Map (Op) := Minerva.Names.To_Name (Name);
      end Map;

      --  Op_None,
      --  Op_Abs, Op_Not, Op_Negate,
      --  Op_Exponent,
      --  Op_Multiply, Op_Divide,
      --  Op_Add, Op_Subtract,
      --  Op_LE, Op_LT, Op_GE, Op_GT,
      --  Op_EQ, Op_NE,
      --  Op_And, Op_Or)
   begin
      Map (Op_Abs, "abs");
      Map (Op_Not, "not");
      Map (Op_Neg, "negate");
      Map (Op_Exp, "**");
      Map (Op_Mul, "*");
      Map (Op_Div, "/");
      Map (Op_Add, "+");
      Map (Op_Sub, "-");
      Map (Op_LE, "<=");
      Map (Op_LT, "<");
      Map (Op_GE, ">=");
      Map (Op_GT, ">");
      Map (Op_EQ, "=");
      Map (Op_NE, "/=");
      Map (Op_And, "and");
      Map (Op_Or, "or");
   end;
end Minerva.Operators;
