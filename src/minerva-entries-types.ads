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

   overriding function Get_Partial
     (This : not null access constant Instance)
      return Minerva.Partials.Reference
   is (raise Constraint_Error with "cannot get partial for type entry");

   overriding procedure Pop
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding procedure Push
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding procedure Push_Address
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

end Minerva.Entries.Types;
