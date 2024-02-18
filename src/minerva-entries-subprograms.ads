private with Ada.Containers.Doubly_Linked_Lists;

with Minerva.Entries.Value.Formal_Arguments;
with Minerva.Operators;
with Minerva.Trees;
with Minerva.Types.Callable;

package Minerva.Entries.Subprograms is

   subtype Parent is Entries.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   type Constant_Reference is access constant Instance;
   type Constant_Class_Reference is access constant Class;

   function Create
     (Declaration      : not null access Minerva.Trees.Class;
      Subprogram_Name  : Minerva.Names.Minerva_Name;
      Container_Name   : Minerva.Names.Minerva_Name;
      Call_Type        : not null Minerva.Types.Callable.Class_Reference)
      return Class_Reference;

   function Create_Operator_Function
     (Declaration    : not null access Minerva.Trees.Class;
      Operator       : Minerva.Operators.Minerva_Operator;
      Call_Type      : not null Minerva.Types.Callable.Class_Reference)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   package Formal_Argument_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Element_Type =>
           Minerva.Entries.Value.Formal_Arguments.Constant_Class_Reference,
        "="          => Minerva.Entries.Value.Formal_Arguments."=");

   type Instance is new Parent with
      record
         Argument_List : Formal_Argument_Lists.List;
         Operator      : Minerva.Operators.Minerva_Operator :=
                           Minerva.Operators.Op_None;
      end record;

   overriding function Get_Partial
     (This : not null access constant Instance)
      return Minerva.Partials.Reference;

   overriding procedure Pop
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding procedure Push
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding procedure Push_Address
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

   procedure Initialize_Subprogram
     (This           : in out Class;
      Declaration    : not null access Minerva.Trees.Class;
      Name           : Minerva.Names.Minerva_Name;
      Container_Name : Minerva.Names.Minerva_Name;
      Call_Type      : not null Minerva.Types.Callable.Class_Reference);

end Minerva.Entries.Subprograms;
