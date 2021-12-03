private with Minerva.Names;

with Minerva.Trees.Expressions;
with Minerva.Trees.Statement_Sequences;

package Minerva.Trees.Statements.Loop_Statement is

   subtype Parent is Statements.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   function Create_Loop
     (Position : GCS.Positions.File_Position;
      Name     : String;
      Sequence : Minerva.Trees.Statement_Sequences.Class_Reference)
      return Class_Reference;

   function Create_While_Loop
     (Position  : GCS.Positions.File_Position;
      Name      : String;
      Condition : Minerva.Trees.Expressions.Class_Reference;
      Sequence  : Minerva.Trees.Statement_Sequences.Class_Reference)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Loop_Form is
     (Basic_Loop, For_Loop, Iterator_Loop, While_Loop);

   type Instance is new Parent with
      record
         Form      : Loop_Form;
         Name      : Minerva.Names.Minerva_Name;
         Condition : Minerva.Trees.Expressions.Class_Reference;
         Loop_Body : Minerva.Trees.Statement_Sequences.Class_Reference;
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

end Minerva.Trees.Statements.Loop_Statement;
