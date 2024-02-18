private with Minerva.Names;

with Minerva.Trees.Blocks;
with Minerva.Trees.Context.Sequence;
with Minerva.Trees.Declarations.Objects.Sequence;
with Minerva.Trees.Types;

package Minerva.Trees.Declarations.Subprograms is

   subtype Parent is Declarations.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   function Create_Specification
     (Position         : GCS.Positions.File_Position;
      Context          : Minerva.Trees.Context.Sequence.Class_Reference;
      Defining_Name    : String;
      Formal_Arguments : Objects.Sequence.Class_Reference;
      Return_Type      : Minerva.Trees.Types.Class_Reference)
      return Class_Reference;

   function Create_Declaration
     (Position         : GCS.Positions.File_Position;
      Context          : Minerva.Trees.Context.Sequence.Class_Reference;
      Defining_Name    : String;
      Formal_Arguments : Objects.Sequence.Class_Reference;
      Return_Type      : Minerva.Trees.Types.Class_Reference;
      Subprogram_Body  : Minerva.Trees.Blocks.Class_Reference)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Has_Body         : Boolean;
         Is_Function      : Boolean;
         Has_Context      : Boolean;
         Argument_Words   : Natural := 0;
         Context          : Minerva.Trees.Context.Sequence.Class_Reference;
         Defining_Name    : Minerva.Names.Minerva_Name;
         Argument_Env     : Minerva.Environment.Environment_Id;
         Formal_Arguments : Objects.Sequence.Class_Reference;
         Return_Type      : Minerva.Trees.Types.Class_Reference;
         Subprogram_Body  : Minerva.Trees.Blocks.Class_Reference;
      end record;

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Compile_Tree
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding function Children (This : Instance) return Class_Reference_Array;

   overriding function Image (This : Instance) return String;

end Minerva.Trees.Declarations.Subprograms;
