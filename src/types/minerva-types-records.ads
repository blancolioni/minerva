with Minerva.Environment;

with Minerva.Types.Composite;

package Minerva.Types.Records is

   subtype Parent is Composite.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   type Constant_Reference is access all Instance;
   type Constant_Class_Reference is access all Class;

   function Create
     (Definition            : not null access Minerva.Trees.Class;
      Component_Environment : Minerva.Environment.Environment_Id)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Components : Minerva.Environment.Environment_Id;
      end record;

   overriding function Description
     (This : Instance)
      return String
   is ("a record type");

end Minerva.Types.Records;
