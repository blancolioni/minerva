with Minerva.Trees.Declarations.Objects.Sequence;

package Minerva.Trees.Types.Records is

   subtype Parent is Types.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   function Create
     (Position   : GCS.Positions.File_Position;
      Components : Trees.Declarations.Objects.Sequence.Class_Reference)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Components : Trees.Declarations.Objects.Sequence.Class_Reference;
      end record;

   overriding function Children (This : Instance) return Class_Reference_Array
   is (Maybe (This.Components));

   overriding function Image (This : Instance) return String;

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Compile_Tree
     (This : Instance;
      Unit : in out Tagatha.Code.Instance);

end Minerva.Trees.Types.Records;
