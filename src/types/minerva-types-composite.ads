package Minerva.Types.Composite is

   subtype Parent is Types.Instance;

   type Instance is abstract new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access constant Instance;
   type Class_Reference is access constant Class;

private

   subtype Dispatch is Instance'Class;

   type Instance is abstract new Parent with
      record
         null;
      end record;

end Minerva.Types.Composite;
