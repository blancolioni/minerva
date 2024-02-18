private with Ada.Containers.Doubly_Linked_Lists;

with Minerva.Entries;
with Minerva.Ids;
with Minerva.Names;
with Minerva.Partials;
with Minerva.Types.Lists;
with Minerva.Values;

package Minerva.Trees.Expressions is

   subtype Parent is Trees.Instance;

   type Instance is
     abstract new Parent
   with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Ids.Environment_Id);

   overriding procedure Compile_Tree
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

   function Get_Partial
     (This : Instance)
      return Minerva.Partials.Reference
      is abstract;

   procedure Push
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is abstract;

   procedure Push_Address
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is abstract;

   procedure Pop
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is abstract;

   function Is_Static
     (This        : Instance;
      Environment : Minerva.Environment.Environment_Id)
      return Boolean
      is abstract;

   function Evaluate
     (This        : Instance;
      Environment : Minerva.Environment.Environment_Id)
      return Minerva.Values.Minerva_Value
      is abstract
     with Pre'Class => This.Is_Static (Environment);

   function Constrain_Types
     (This           : in out Instance;
      Possible_Types : Minerva.Types.Lists.List;
      Environment    : Minerva.Environment.Environment_Id)
      return Minerva.Types.Lists.List
      is abstract;

   procedure Add_Possible_Type
     (This          : in out Class;
      Possible_Type : not null Minerva.Types.Class_Reference);

   procedure Set_Type
     (This          : in out Instance;
      Possible_Type : not null Minerva.Types.Class_Reference);

   function Has_Type
     (This : Class)
      return Boolean;

   function Get_Type
     (This : Class)
      return Minerva.Types.Class_Reference;

   function To_List
     (Single_Type : Minerva.Types.Class_Reference)
      return Minerva.Types.Lists.List;

   procedure Constrain_Type
     (This           : in out Instance;
      Possible_Types : Minerva.Types.Lists.List);

   procedure Constrain_Type
     (This          : in out Class;
      Possible_Type : Minerva.Types.Class_Reference);

   function Is_Compatible
     (Available_Type : Minerva.Types.Class_Reference;
      List           : Minerva.Types.Lists.List)
      return Boolean;

   function Possible_Types
     (This : Class)
      return Minerva.Types.Lists.List;

private

   subtype Dispatch is Instance'Class;

   package Matching_Entry_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Minerva.Entries.Constant_Class_Reference, Minerva.Entries."=");

   type Instance is
     abstract new Parent with
      record
         Expression_Type  : Minerva.Types.Class_Reference;
         Possible_Types   : Minerva.Types.Lists.List;
         Matching_Entries : Matching_Entry_Lists.List;
      end record;

   function Constrain_Identifier
     (This           : in out Instance;
      Identifier     : Minerva.Names.Minerva_Name;
      Possible_Types : Minerva.Types.Lists.List;
      Environment    : Minerva.Environment.Environment_Id)
         return Minerva.Types.Lists.List;

   function Possible_Types
     (This : Class)
         return Minerva.Types.Lists.List
   is (This.Possible_Types);

   function Has_Type
     (This : Class)
         return Boolean
   is (Minerva.Types."/=" (This.Expression_Type, null));

end Minerva.Trees.Expressions;
