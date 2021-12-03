with Tagatha;

with Minerva.Trees;
with Minerva.Types;

package Minerva.Entries.Value.Literals is

   subtype Parent is Value.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   type Constant_Reference is access all Instance;
   type Constant_Class_Reference is access all Class;

   function Create
     (Declaration   : not null access Minerva.Trees.Class;
      Literal_Name  : Minerva.Names.Minerva_Name;
      Literal_Type  : Minerva.Types.Class_Reference;
      Literal_Value : Natural)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Value : Natural;
      end record;

   overriding procedure Pop
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   overriding procedure Push
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   overriding procedure Push_Address
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit);

end Minerva.Entries.Value.Literals;
