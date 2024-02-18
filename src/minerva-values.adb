with Minerva.Primitives;

package body Minerva.Values is

   function To_Float (Value : Minerva_Value) return Float
   is (case Value.Class is
          when No_Value => 0.0,
          when Signed_Integer_Class | Unsigned_Integer_Class =>
             Float (Value.Integral_Value),
          when Floating_Point_Class                          =>
             Value.Float_Value);

   function Operate
     (Operator : Minerva.Operators.Minerva_Operator;
      Value_Type  : Minerva.Types.Class_Reference;
      Left, Right : Float)
      return Minerva_Value;

   function Operate
     (Operator    : Minerva.Operators.Minerva_Operator;
      Value_Type  : Minerva.Types.Class_Reference;
      Left, Right : Integer)
      return Minerva_Value;

   --------------------------
   -- Floating_Point_Value --
   --------------------------

   function Floating_Point_Value
     (Value_Type : Minerva.Types.Class_Reference;
      Value      : Float)
      return Minerva_Value
   is
   begin
      return Minerva_Value'
        (Class          => Floating_Point_Class,
         Value_Type     => Value_Type,
         Float_Value    => Value);
   end Floating_Point_Value;

   -------------
   -- Operate --
   -------------

   function Operate
     (Operator    : Minerva.Operators.Minerva_Operator;
      Left, Right : Minerva_Value)
      return Minerva_Value
   is
   begin
      if Left.Class = No_Value or else Right.Class = No_Value then
         return Left;
      elsif Left.Class = Floating_Point_Class
        or else Right.Class = Floating_Point_Class
      then
         return Operate (Operator, Left.Value_Type,
                         To_Float (Left), To_Float (Right));
      else
         return Operate (Operator,
                         Left.Value_Type,
                         Left.Integral_Value, Right.Integral_Value);
      end if;
   end Operate;

   -------------
   -- Operate --
   -------------

   function Operate
     (Operator    : Minerva.Operators.Minerva_Operator;
      Value_Type  : Minerva.Types.Class_Reference;
      Left, Right : Integer)
      return Minerva_Value
   is
      use all type Minerva.Operators.Minerva_Operator;

      function I (X : Integer) return Minerva_Value
      is (if Value_Type.Is_Signed
          then (Signed_Integer_Class, Value_Type, X)
          else (Unsigned_Integer_Class, Value_Type, X));

      function B (X : Boolean) return Minerva_Value
      is (Signed_Integer_Class, Minerva.Primitives.Standard_Boolean,
          Boolean'Pos (X));

   begin
      return (case Operator is
                 when Op_None => I (Left),
                 when Op_Abs  => I (abs Left),
                 when Op_Not  => B (Left = 0),
                 when Op_Neg  => I (-Left),
                 when Op_Exp  => I (Left ** Right),
                 when Op_Mul  => I (Left * Right),
                 when Op_Div  => (if Right = 0
                                  then raise Constraint_Error with
                                    "division by zero"
                                  else I (Left / Right)),
                 when Op_Mod  => (if Right = 0
                                  then raise Constraint_Error with
                                    "division by zero"
                                  else I (Left / Right)),
                 when Op_Add  => I (Left + Right),
                 when Op_Sub  => I (Left - Right),
                 when Op_LE   => B (Left <= Right),
                 when Op_LT   => B (Left < Right),
                 when Op_GE   => B (Left >= Right),
                 when Op_GT   => B (Left > Right),
                 when Op_EQ   => B (Left = Right),
                 when Op_NE   => B (Left /= Right),
                 when Op_And  => B (Left /= 0 and then Right /= 0),
                 when Op_Or   => B (Left /= 0 or else Right /= 0));
   end Operate;

   -------------
   -- Operate --
   -------------

   function Operate
     (Operator    : Minerva.Operators.Minerva_Operator;
      Value_Type  : Minerva.Types.Class_Reference;
      Left, Right : Float)
      return Minerva_Value
   is
      use all type Minerva.Operators.Minerva_Operator;

      function F (X : Float) return Minerva_Value
      is (Floating_Point_Class, Value_Type, X);

      function B (X : Boolean) return Minerva_Value
      is (Signed_Integer_Class, Minerva.Primitives.Standard_Boolean,
          Boolean'Pos (X));

   begin
      return (case Operator is
                 when Op_None => F (Left),
                 when Op_Abs  => F (abs Left),
                 when Op_Not  => (raise Constraint_Error with "bad operator"),
                 when Op_Neg  => F (-Left),
                 when Op_Exp  => F (Left ** Integer (Right)),

                 when Op_Mul => F (Left * Right),
                 when Op_Div => (if Right = 0.0
                                  then raise Constraint_Error with
                                    "division by zero"
                                  else F (Left / Right)),
                 when Op_Add => F (Left + Right),
                 when Op_Sub => F (Left - Right),
                 when Op_Mod =>
                (raise Constraint_Error
                   with "bad floating point operator: "
                 & "mod"),
                 when Op_LE              => B (Left <= Right),
                 when Op_LT   => B (Left < Right),
                 when Op_GE   => B (Left >= Right),
                 when Op_GT   => B (Left > Right),
                 when Op_EQ   => B (Left = Right),
                 when Op_NE   => B (Left /= Right),
                 when Op_And  => (raise Constraint_Error
                                    with "bad floating point operator: "
                                      & "and"),
                 when Op_Or       => (raise Constraint_Error
                                        with "bad floating point operator: "
                                      & "or"));
   end Operate;

   ----------
   -- Push --
   ----------

   procedure Push
     (Value : Minerva_Value;
      Code  : in out Tagatha.Code.Instance'Class)
   is
   begin
      case Value.Class is
         when No_Value =>
            raise Constraint_Error with
              "cannot push no value";
         when Signed_Integer_Class | Unsigned_Integer_Class =>
            Code.Push_Constant (Tagatha.Int_32 (Value.Integral_Value));
         when Floating_Point_Class =>
            Code.Push_Constant
              (Tagatha.Floating_Point_Constant (Value.Float_Value));
      end case;
   end Push;

   --------------------------
   -- Signed_Integer_Value --
   --------------------------

   function Signed_Integer_Value
     (Value_Type : Minerva.Types.Class_Reference;
      Value      : Integer)
      return Minerva_Value
   is
   begin
      return Minerva_Value'
        (Class          => Signed_Integer_Class,
         Value_Type     => Value_Type,
         Integral_Value => Value);
   end Signed_Integer_Value;

   ----------------
   -- To_Address --
   ----------------

   function To_Address
     (Value : Minerva_Value)
      return Minerva.Target.Target_Address_Type
   is
   begin
      pragma Assert
        (Value.Class in Signed_Integer_Class | Unsigned_Integer_Class,
         "expected an integral value for Address");
      pragma Assert
        (Value.Integral_Value >= 0,
         "address must be non-negative");
      pragma Assert
        (Value.Integral_Value < Minerva.Target.Target_Address_Type'Modulus,
         "address must in in the range 0 .."
         & Minerva.Target.Target_Address_Type'Modulus'Image);
      return Minerva.Target.Target_Address_Type (Value.Integral_Value);
   end To_Address;

   ----------------------------
   -- Unsigned_Integer_Value --
   ----------------------------

   function Unsigned_Integer_Value
     (Value_Type : Minerva.Types.Class_Reference;
      Value      : Natural)
      return Minerva_Value
   is
   begin
      return Minerva_Value'
        (Class          => Unsigned_Integer_Class,
         Value_Type     => Value_Type,
         Integral_Value => Value);
   end Unsigned_Integer_Value;

end Minerva.Values;
