with Minerva.Trees.Expressions;

package Minerva.Trees.Statements.Assignment_Statement is

   subtype Parent is Statements.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   function Create
     (Position : GCS.Positions.File_Position;
      Target   : Minerva.Trees.Expressions.Class_Reference;
      Value    : Minerva.Trees.Expressions.Class_Reference)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Target   : Minerva.Trees.Expressions.Class_Reference;
         Value    : Minerva.Trees.Expressions.Class_Reference;
      end record;

   overriding function Image
     (This : Instance)
      return String;

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Compile_Tree
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding function Children
     (This    : Instance)
      return Class_Reference_Array;

end Minerva.Trees.Statements.Assignment_Statement;
