private with Ada.Containers.Doubly_Linked_Lists;

with Minerva.Names;
with Minerva.Environment;

with Minerva.Trees.Declarations;
with Minerva.Trees.Statements;

package Minerva.Trees.Blocks is

   subtype Parent is Trees.Instance;

   type Instance is new Parent
     and Minerva.Names.Name_Interface
   with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Compile_Tree
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   overriding function Children
     (This    : Instance)
      return Class_Reference_Array;

   procedure Add_Declaration
     (This        : in out Class;
      Declaration : not null Minerva.Trees.Declarations.Class_Reference);

   procedure Add_Statement
     (This      : in out Class;
      Statement : not null Minerva.Trees.Statements.Class_Reference);

   function Create_Block
     (Position : GCS.Positions.File_Position;
      Name     : String)
      return Class_Reference;

private

   package Declaration_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Minerva.Trees.Declarations.Class_Reference,
        Minerva.Trees.Declarations."=");

   package Statement_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Minerva.Trees.Statements.Class_Reference,
        Minerva.Trees.Statements."=");

   subtype Dispatch is Instance'Class;

   type Instance is new Parent
     and Minerva.Names.Name_Interface with
      record
         Name         : Minerva.Names.Minerva_Name;
         Declarations : Declaration_Lists.List;
         Statements   : Statement_Lists.List;
      end record;

   overriding function Name
     (This : Instance)
      return Minerva.Names.Minerva_Name
   is (This.Name);

   overriding function Image
     (This : Instance)
      return String;

end Minerva.Trees.Blocks;
