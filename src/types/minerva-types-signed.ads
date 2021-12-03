with Minerva.Values;

with Minerva.Types.Integral;

package Minerva.Types.Signed is

   subtype Parent is Integral.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   type Constant_Reference is access all Instance;
   type Constant_Class_Reference is access all Class;

   function Create
     (Definition : not null access Minerva.Trees.Class;
      Name       : String;
      First, Last : Integer)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         First, Last : Minerva.Values.Minerva_Value;
      end record;

   overriding function Description
     (This : Instance)
      return String
   is ("a signed integer type");

end Minerva.Types.Signed;
