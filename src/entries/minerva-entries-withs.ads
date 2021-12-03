with Minerva.Ids;
with Minerva.Trees;

package Minerva.Entries.Withs is

   subtype Parent is Entries.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   type Constant_Reference is access constant Instance;
   type Constant_Class_Reference is access constant Class;

   function Child_Environment
     (This : Class)
      return Minerva.Ids.Environment_Id;

   function Create
     (Declaration  : not null access Minerva.Trees.Class;
      Package_Name : String;
      Package_Env  : Minerva.Ids.Environment_Id)
      return Constant_Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Package_Env : Minerva.Ids.Environment_Id;
         Environment : Minerva.Ids.Environment_Id;
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

end Minerva.Entries.Withs;
