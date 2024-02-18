with Minerva.Types.Scalar;

package Minerva.Types.Discrete is

   subtype Parent is Scalar.Instance;

   type Instance is abstract new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   type Constant_Reference is access all Instance;
   type Constant_Class_Reference is access all Class;

private

   subtype Dispatch is Instance'Class;

   type Instance is abstract new Parent with
      record
         null;
      end record;

end Minerva.Types.Discrete;
