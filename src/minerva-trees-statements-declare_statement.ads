with Minerva.Trees.Blocks;

package Minerva.Trees.Statements.Declare_Statement is

   subtype Parent is Statements.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   function Create
     (Position : GCS.Positions.File_Position;
      Block    : Minerva.Trees.Blocks.Class_Reference)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Block    : Minerva.Trees.Blocks.Class_Reference;
      end record;

   overriding function Image
     (This : Instance)
      return String
   is ("declare " & This.Block.Image);

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Compile_Tree
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding function Children
     (This    : Instance)
      return Class_Reference_Array
   is (Maybe (This.Block));

end Minerva.Trees.Statements.Declare_Statement;
