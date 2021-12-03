private with Ada.Containers.Doubly_Linked_Lists;

with Minerva.Ids;

with Minerva.Trees;

with Minerva.Types.Discrete;

package Minerva.Types.Enumerated is

   subtype Parent is Discrete.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   type Constant_Reference is access all Instance;
   type Constant_Class_Reference is access all Class;

   overriding procedure Elaborate
     (This        : Instance;
      Environment : Minerva.Ids.Environment_Id);

   function Create_Enumerated_Type
     (Definition : not null access Minerva.Trees.Class;
      Name       : String)
      return Class_Reference;

   procedure Add_Literal
     (This        : in out Class;
      Declaration : not null access Minerva.Trees.Class;
      Name        : String);

   procedure Add_Literal
     (This        : in out Class;
      Declaration : not null access Minerva.Trees.Class;
      Name        : String;
      Value       : Natural);

   function Is_Literal
     (This    : Class;
      Literal : String)
      return Boolean;

   function Literal_Value
     (This    : Class;
      Literal : String)
      return Natural;

private

   type Literal_Record is
      record
         Declaration : Minerva.Trees.Class_Reference;
         Identifier  : Minerva.Names.Minerva_Name;
         Value       : Natural;
      end record;

   package Literal_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Literal_Record);

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Literals      : Literal_Lists.List;
         Literal_Type  : Minerva.Types.Class_Reference;
      end record;

   overriding function Description
     (This : Instance)
      return String
   is ("an enumerated type");

end Minerva.Types.Enumerated;
