with Minerva.Trees;
with Minerva.Types;

package Minerva.Entries.Types is

   subtype Parent is Entries.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   type Constant_Reference is access all Instance;
   type Constant_Class_Reference is access all Class;

   function Create
     (Declaration : not null access Minerva.Trees.Class;
      Name        : Minerva.Names.Minerva_Name;
      Definition  : not null access constant Minerva.Types.Class)
      return Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Type_Definition : Minerva.Types.Class_Reference;
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

end Minerva.Entries.Types;
