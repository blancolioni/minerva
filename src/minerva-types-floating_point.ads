with Minerva.Types.Real;

package Minerva.Types.Floating_Point is

   subtype Parent is Real.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   type Constant_Reference is access all Instance;
   type Constant_Class_Reference is access all Class;

   function Create
     (Definition : not null access Minerva.Trees.Class;
      Name       : String;
      Num_Digits : Positive)
      return Class_Reference;

   function Standard_Float return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Num_Digits : Positive;
      end record;

   overriding function Description
     (This : Instance)
      return String
   is ("a floating point type");

   overriding function Content
     (This : Instance)
      return Tagatha.Operand_Content
   is (Tagatha.Floating_Point_Content);

end Minerva.Types.Floating_Point;
