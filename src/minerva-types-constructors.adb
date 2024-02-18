with Minerva.Types.Modular;
with Minerva.Types.Signed;
with Minerva.Types.Floating_Point;

package body Minerva.Types.Constructors is

   -------------------------
   -- Create_Modular_Type --
   -------------------------

   function Create_Modular_Type
     (Definition  : not null access Minerva.Trees.Class;
      Name        : String;
      Modulus     : Positive)
      return Class_Reference
   is
      use Minerva.Types.Modular;
   begin
      return Minerva.Types.Class_Reference
        (Create (Definition, Name, Modulus));
   end Create_Modular_Type;

   --------------------------------
   -- Create_Signed_Integer_Type --
   --------------------------------

   function Create_Signed_Integer_Type
     (Definition  : not null access Minerva.Trees.Class;
      Name        : String;
      First, Last : Integer)
      return Class_Reference
   is
      use Minerva.Types.Signed;
   begin
      return Minerva.Types.Class_Reference
        (Create (Definition, Name, First, Last));
   end Create_Signed_Integer_Type;

   ---------------------------------
   -- Single_Precision_Float_Type --
   ---------------------------------

   function Single_Precision_Float_Type
      return Class_Reference
   is
   begin
      return Class_Reference
        (Minerva.Types.Floating_Point.Standard_Float);
   end Single_Precision_Float_Type;

end Minerva.Types.Constructors;
