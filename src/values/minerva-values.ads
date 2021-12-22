with Tagatha.Code;

with Minerva.Operators;
with Minerva.Target;
with Minerva.Types;

private package Minerva.Values is

   type Minerva_Value is private;

   function Signed_Integer_Value
     (Value_Type : Minerva.Types.Class_Reference;
      Value      : Integer)
      return Minerva_Value;

   function Unsigned_Integer_Value
     (Value_Type : Minerva.Types.Class_Reference;
      Value      : Natural)
      return Minerva_Value;

   function Floating_Point_Value
     (Value_Type : Minerva.Types.Class_Reference;
      Value      : Float)
      return Minerva_Value;

   function To_Address
     (Value : Minerva_Value)
      return Minerva.Target.Target_Address_Type;

   function Operate
     (Operator    : Minerva.Operators.Minerva_Operator;
      Left, Right : Minerva_Value)
      return Minerva_Value;

   procedure Push
     (Value : Minerva_Value;
      Unit  : in out Tagatha.Code.Instance);

private

   type Value_Class is
     (No_Value, Signed_Integer_Class, Unsigned_Integer_Class,
      Floating_Point_Class);

   type Minerva_Value (Class : Value_Class := No_Value) is
      record
         Value_Type : Minerva.Types.Class_Reference;
         case Class is
            when No_Value =>
               null;
            when Signed_Integer_Class | Unsigned_Integer_Class =>
               Integral_Value : Integer;
            when Floating_Point_Class =>
               Float_Value : Float;
         end case;
      end record;

end Minerva.Values;
