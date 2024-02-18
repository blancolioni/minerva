with GCS.Positions;

with Minerva.Parser.Tokens;             use Minerva.Parser.Tokens;
with Minerva.Parser.Lexical;            use Minerva.Parser.Lexical;

with Minerva.Names;
with Minerva.Operators;

with Minerva.Trees.Expressions.Calls;
with Minerva.Trees.Expressions.Constants;
with Minerva.Trees.Expressions.Dots;
with Minerva.Trees.Expressions.Identifiers;

package body Minerva.Parser.Expressions is

   subtype Expression is Minerva.Trees.Expressions.Class_Reference;

   use all type Minerva.Operators.Minerva_Operator;

   type Operator_Precedence is range 1 .. 9;

   type Operator_Record is
      record
         Valid      : Boolean := False;
         Op         : Minerva.Operators.Minerva_Operator :=
                        Minerva.Operators.Op_None;
         Precedence : Operator_Precedence := 1;
         Prefix     : Boolean := False;
      end record;

   Operator_Table : constant array (Token) of Operator_Record :=
                      [Tok_Asterisk      => (True, Op_Mul, 6, False),
                       Tok_Slash         => (True, Op_Div, 6, False),
                       Tok_Mod           => (True, Op_Mod, 6, False),
                       Tok_Plus          => (True, Op_Add, 5, False),
                       Tok_Minus         => (True, Op_Sub, 5, False),
                       Tok_Less_Equal    => (True, Op_LE, 4, False),
                       Tok_Less          => (True, Op_LT, 4, False),
                       Tok_Greater_Equal => (True, Op_GE, 4, False),
                       Tok_Greater       => (True, Op_GT, 4, False),
                       Tok_Equal         => (True, Op_EQ, 3, False),
                       Tok_Not_Equal     => (True, Op_NE, 3, False),
                       Tok_And           => (True, Op_And, 2, False),
                       Tok_Or            => (True, Op_Or, 1, False),
                       others            => <>];

   function Parse_Operator_Expression
     (Precedence : Operator_Precedence)
      return Expression;

   function Parse_Primary_Expression
     return Expression;

   function Parse_Name return Expression;

   -------------------
   -- At_Expression --
   -------------------

   function At_Expression return Boolean is
   begin
      return Tok = Tok_Integer_Constant
        or else Tok = Tok_Floating_Point_Constant
        or else Tok = Tok_Character
        or else Tok = Tok_Identifier;
   end At_Expression;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression
     return Minerva.Trees.Expressions.Class_Reference
   is
   begin
      return Parse_Operator_Expression (1);
   end Parse_Expression;

   ----------------
   -- Parse_Name --
   ----------------

   function Parse_Name return Expression is
      pragma Assert (Tok = Tok_Identifier);

      Position : constant GCS.Positions.File_Position :=
                   Get_Current_Position;

      Result : Expression :=
                 Expression
                   (Minerva.Trees.Expressions.Identifiers
                    .Create_Identifier_Expression
                      (Position, Minerva.Names.To_Name (Tok_Text)));

      function Parse_Actual_Arguments
        return Minerva.Trees.Expressions.Calls.Actual_Argument_Array;

      ----------------------------
      -- Parse_Actual_Arguments --
      ----------------------------

      function Parse_Actual_Arguments
        return Minerva.Trees.Expressions.Calls.Actual_Argument_Array
      is
         use Minerva.Trees.Expressions.Calls;
      begin
         if At_Expression then
            declare
               Expr : constant Expression := Parse_Expression;
            begin
               if Tok = Tok_Comma then
                  Scan;
                  if At_Expression then
                     return Expr & Parse_Actual_Arguments;
                  else
                     Error ("missing argument");
                     return [Expr];
                  end if;
               else
                  if Tok = Tok_Right_Paren then
                     Scan;
                     return [Expr];
                  elsif At_Expression then
                     Error ("missing ','");
                     return Expr & Parse_Actual_Arguments;
                  else
                     Error ("missing ')'");
                     return [Expr];
                  end if;
               end if;
            end;
         else
            return [];
         end if;
      end Parse_Actual_Arguments;

   begin
      Scan;
      while Tok = Tok_Left_Paren
        or else Tok = Tok_Dot
      loop
         if Tok = Tok_Left_Paren then
            Scan;
            declare
               Args : constant Minerva.Trees.Expressions.Calls
                 .Actual_Argument_Array
                   := Parse_Actual_Arguments;
            begin
               Result :=
                 Expression
                   (Minerva.Trees.Expressions.Calls.Create_Call_Expression
                      (Position  => Position,
                       Call      => Result,
                       Arguments => Args));
            end;
         elsif Tok = Tok_Dot then
            Scan;
            if Tok = Tok_Identifier then
               Result := Expression
                 (Minerva.Trees.Expressions.Dots.Create_Dot_Expression
                    (Position => Position,
                     Left     => Result,
                     Right    => Tok_Text));
               Scan;
            else
               Error ("missing identifier");
            end if;
         end if;
      end loop;
      return Result;
   end Parse_Name;

   -------------------------------
   -- Parse_Operator_Expression --
   -------------------------------

   function Parse_Operator_Expression
     (Precedence : Operator_Precedence)
      return Minerva.Trees.Expressions.Class_Reference
   is
      Position : constant GCS.Positions.File_Position :=
                   Get_Current_Position;

      Left : Expression :=
               (if Precedence = Operator_Precedence'Last
                then Parse_Primary_Expression
                else Parse_Operator_Expression (Precedence + 1));
   begin
      while Operator_Table (Tok).Valid
        and then Operator_Table (Tok).Precedence = Precedence
      loop
         declare
            Op        : constant Minerva.Operators.Minerva_Operator :=
                          Operator_Table (Tok).Op;
            Op_Name   : constant String :=
                          Minerva.Names.Standard_Text
                            (Minerva.Operators.Get_Name (Op));
            Op_Expr   : constant Minerva.Trees.Expressions.Identifiers
              .Class_Reference
                := Minerva.Trees.Expressions.Identifiers
                  .Create_Identifier_Expression
                    (Position   => Get_Current_Position,
                     Identifier => Minerva.Names.To_Name (Op_Name));
            Right     : Expression;
            Call_Expr : Minerva.Trees.Expressions.Calls.Class_Reference;
         begin
            Scan;
            Right :=
              (if Precedence = Operator_Precedence'Last
               then Parse_Primary_Expression
               else Parse_Operator_Expression (Precedence + 1));
            Call_Expr :=
              Minerva.Trees.Expressions.Calls.Create_Call_Expression
                (Position  => Position,
                 Call      => Expression (Op_Expr),
                 Arguments => [Left, Right]);
            Left := Expression (Call_Expr);
         end;
      end loop;
      return Left;
   end Parse_Operator_Expression;

   ------------------------------
   -- Parse_Primary_Expression --
   ------------------------------

   function Parse_Primary_Expression
      return Expression
   is
   begin
      if Tok = Tok_Integer_Constant then
         declare
            use Minerva.Trees.Expressions.Constants;
            Result : constant Class_Reference :=
                       Create_Universal_Integer
                         (Position => Get_Current_Position,
                          Image    => Tok_Text);
         begin
            Scan;
            return Expression (Result);
         end;
      elsif Tok = Tok_Floating_Point_Constant then
         declare
            use Minerva.Trees.Expressions.Constants;
            Result : constant Class_Reference :=
                       Create_Universal_Float
                         (Position => Get_Current_Position,
                          Image    => Tok_Text);
         begin
            Scan;
            return Expression (Result);
         end;
      elsif Tok = Tok_Character then
         declare
            use Minerva.Trees.Expressions.Identifiers;
            Result : constant Class_Reference :=
                       Create_Identifier_Expression
                         (Position   => Get_Current_Position,
                          Identifier =>
                            Minerva.Names.To_Name
                              ("'" & Tok_Character_Value & "'",
                               Case_Sensitive => True));
         begin
            Scan;
            return Expression (Result);
         end;
      elsif Tok = Tok_Identifier then
         return Parse_Name;
      else
         raise Constraint_Error with
           "Minerva.Parser.Expressions: precondition violated";
      end if;

   end Parse_Primary_Expression;

end Minerva.Parser.Expressions;
