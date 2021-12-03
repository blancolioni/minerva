private with Ada.Containers.Doubly_Linked_Lists;
private with Minerva.Entries;
private with Minerva.Names;
private with Minerva.Trees.Declarations;

package Minerva.Trees.Context is

   subtype Parent is Trees.Instance;
   type Instance is new Parent with private;
   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   function Create_With_Context
     (Position : GCS.Positions.File_Position;
      Name     : String)
      return Class_Reference;

   function Create_Use_Context
     (Position : GCS.Positions.File_Position;
      Name     : String)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Context_Clause_Type is (With_Context, Use_Context);

   package Library_Spec_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Minerva.Trees.Declarations.Class_Reference,
        Minerva.Trees.Declarations."=");

   type Instance is new Parent with
      record
         Context_Clause : Context_Clause_Type;
         Name           : Minerva.Names.Minerva_Name;
         Library_Specs  : Library_Spec_Lists.List;
         Library_Spec   : Minerva.Trees.Declarations.Class_Reference;
         Library_Body   : Minerva.Trees.Declarations.Class_Reference;
         Library_Entry  : Minerva.Entries.Constant_Class_Reference;
      end record;

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Compile_Tree
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   overriding function Children
     (This    : Instance)
      return Class_Reference_Array;

   overriding function Image
     (This : Instance)
      return String;

end Minerva.Trees.Context;
