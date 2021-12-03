with Tagatha;

with Minerva.Target;
with Minerva.Types;

package Minerva.Entries.Value.Objects is

   subtype Parent is Value.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   type Constant_Reference is access all Instance;
   type Constant_Class_Reference is access all Class;

   function Create_With_Local_Offset
     (Declaration : not null access Minerva.Trees.Class;
      Object_Name : Minerva.Names.Minerva_Name;
      Object_Type : Minerva.Types.Class_Reference;
      Offset      : Tagatha.Local_Offset)
      return Class_Reference;

   function Create_With_Dynamic_Address
     (Declaration : not null access Minerva.Trees.Class;
      Object_Name : Minerva.Names.Minerva_Name;
      Object_Type : Minerva.Types.Class_Reference;
      Offset      : Tagatha.Local_Offset;
      Address     : Minerva.Trees.Expressions.Class_Reference)
      return Class_Reference;

   function Create_With_Static_Address
     (Declaration : not null access Minerva.Trees.Class;
      Object_Name : Minerva.Names.Minerva_Name;
      Object_Type : Minerva.Types.Class_Reference;
      Address     : Minerva.Target.Target_Address_Type)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Variable_State_Access is access Boolean;

   type Instance is new Parent with
      record
         Address          : Minerva.Trees.Expressions.Class_Reference;
         Static_Address   : Minerva.Target.Target_Address_Type := 0;
         Local            : Tagatha.Local_Offset;
         Variable_State   : Variable_State_Access;
      end record;

   overriding function Is_Object_Entry
     (This : Instance)
      return Boolean
   is (True);

   overriding procedure Pop
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   overriding procedure Push
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   overriding procedure Push_Address
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   overriding function Static_Address
     (This : Instance)
      return Minerva.Target.Target_Address_Type
   is (This.Static_Address);

end Minerva.Entries.Value.Objects;
