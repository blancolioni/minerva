with Minerva.Environment;

package Minerva.Trees.Statements.Null_Statement is

   subtype Parent is Statements.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   function Create
     (Position : GCS.Positions.File_Position)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         null;
      end record;

   overriding function Image
     (This : Instance)
      return String
   is ("null");

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id)
   is null;

   overriding procedure Compile_Tree
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is null;

   overriding function Children
     (This    : Instance)
      return Class_Reference_Array
   is (Null_Class_Reference_Array);

end Minerva.Trees.Statements.Null_Statement;
