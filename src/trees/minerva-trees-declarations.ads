private with Ada.Containers.Doubly_Linked_Lists;

with Minerva.Trees.Aspects;

package Minerva.Trees.Declarations is

   subtype Parent is Trees.Instance;

   type Instance is abstract new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   procedure Add_Aspect
     (This   : in out Class;
      Aspect : not null Minerva.Trees.Aspects.Class_Reference);

   procedure Iterate_Entries
     (This : Instance;
      Process : not null access
        procedure (Item : Minerva.Entries.Constant_Class_Reference));

private

   package Aspect_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Minerva.Trees.Aspects.Class_Reference,
        Minerva.Trees.Aspects."=");

   subtype Dispatch is Instance'Class;

   type Instance is abstract new Parent with
      record
         Aspects    : Aspect_Lists.List;
      end record;

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id);

   function Has_Aspect
     (This : Class;
      Name : String)
      return Boolean;

   function Find_Aspect
     (This : Class;
      Name : String)
      return Minerva.Trees.Aspects.Class_Reference;

   package List_Of_Declarations is
     new Ada.Containers.Doubly_Linked_Lists (Class_Reference);

end Minerva.Trees.Declarations;
