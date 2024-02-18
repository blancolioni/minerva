private with Minerva.Names;

with Minerva.Entries;
with Minerva.Environment;
with Minerva.Trees.Context.Sequence;

package Minerva.Trees.Declarations.Packages is

   subtype Parent is Declarations.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   procedure Append
     (This        : in out Class;
      Declaration : Minerva.Trees.Declarations.Class_Reference);

   function Create_Package_Specification
     (Position      : GCS.Positions.File_Position;
      Context       : Minerva.Trees.Context.Sequence.Class_Reference;
      Defining_Name : String)
      return Class_Reference;

   function Create_Package_Body
     (Position      : GCS.Positions.File_Position;
      Context       : Minerva.Trees.Context.Sequence.Class_Reference;
      Defining_Name : String)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Is_Body       : Boolean;
         Context       : Minerva.Trees.Context.Sequence.Class_Reference;
         Parent_Names  : Minerva.Names.Minerva_Name;
         Parent_Entry  : Minerva.Entries.Constant_Class_Reference;
         Defining_Name : Minerva.Names.Minerva_Name;
         Children      : List_Of_Declarations.List;
      end record;

   overriding function Image
     (This : Instance)
      return String;

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Compile_Tree
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding function Children
     (This    : Instance)
      return Class_Reference_Array;

end Minerva.Trees.Declarations.Packages;
