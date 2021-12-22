private with Ada.Containers.Doubly_Linked_Lists;

with Minerva.Environment;

with Minerva.Trees;

generic
   type Element_Instance is abstract new Trees.Instance with private;
   type Element_Reference is access all Element_Instance'Class;
package Minerva.Trees.Sequences is

   subtype Parent is Trees.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   overriding function Image
     (This : Instance)
      return String;

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id);

   overriding procedure Compile_Tree
     (This : Instance;
      Unit : in out Tagatha.Code.Instance);

   overriding function Children
     (This    : Instance)
      return Class_Reference_Array;

   function Length
     (This : Class)
      return Natural;

   procedure Append
     (This    : in out Class;
      Element : not null access Element_Instance'Class);

   procedure Iterate
     (This : Class;
      Process : not null access
        procedure (Element : Element_Reference));

   function Create_Sequence
     (Position : GCS.Positions.File_Position)
      return Class_Reference;

private

   package Element_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Reference);

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         List : Element_Lists.List;
      end record;

   function Length
     (This : Class)
      return Natural
   is (Natural (This.List.Length));

end Minerva.Trees.Sequences;
