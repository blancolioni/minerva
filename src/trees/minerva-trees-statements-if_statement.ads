private with Ada.Containers.Doubly_Linked_Lists;

with Minerva.Trees.Expressions;
with Minerva.Trees.Statement_Sequences;

package Minerva.Trees.Statements.If_Statement is

   subtype Parent is Statements.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   function Create
     (Position  : GCS.Positions.File_Position)
      return Class_Reference;

   procedure Add_Condition_And_Sequence
     (This      : in out Class;
      Condition : Minerva.Trees.Expressions.Class_Reference;
      Sequence  : Minerva.Trees.Statement_Sequences.Class_Reference);

   procedure Add_Sequence
     (This      : in out Class;
      Sequence  : Minerva.Trees.Statement_Sequences.Class_Reference);

private

   subtype Dispatch is Instance'Class;

   type Element_Type is
      record
         Condition : Minerva.Trees.Expressions.Class_Reference;
         Sequence  : Minerva.Trees.Statement_Sequences.Class_Reference;
      end record;

   package Element_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type);

   type Instance is new Parent with
      record
         Elements : Element_Lists.List;
      end record;

   overriding function Image
     (This : Instance)
      return String;

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Compile_Tree
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   overriding function Children
     (This    : Instance)
      return Class_Reference_Array;

end Minerva.Trees.Statements.If_Statement;
