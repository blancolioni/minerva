with Tagatha.Units;

with Minerva.Names;

limited with Minerva.Trees;
limited with Minerva.Trees.Expressions;

with Minerva.Types;
with Minerva.Values;

with Minerva.Target;

private package Minerva.Entries is

   type Instance is
     abstract new Minerva.Names.Name_Interface
   with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   type Constant_Reference is access constant Instance;
   type Constant_Class_Reference is access constant Class;

   function Entry_Value
     (This : Instance)
      return Minerva.Values.Minerva_Value;

   procedure Pop
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is abstract;

   procedure Push
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is abstract;

   procedure Push_Address
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is abstract;

   function Link_Name
     (This : Class)
      return String;

   function Declaration
     (This : Class)
      return Minerva.Trees.Class_Reference;

   function Is_Type_Entry
     (This : Class)
      return Boolean;

   function Is_Object_Entry
     (This : Instance)
      return Boolean;

   function Is_Package_Reference
     (This : Instance)
      return Boolean;

   function Is_Constant
     (This : Class)
      return Boolean;

   function Is_Static
     (This : Class)
      return Boolean;

   function Is_Intrinsic
     (This : Class)
      return Boolean;

   function Is_Overloadable
     (This : Class)
      return Boolean;

   function Has_Address
     (This : Class)
      return Boolean;

   function Has_Static_Address
     (This : Instance)
      return Boolean;

   function Static_Address
     (This : Instance)
      return Minerva.Target.Target_Address_Type
     with Pre'Class => This.Has_Static_Address;

   function Has_Initializer
     (This : Class)
      return Boolean;

   function Initializer
     (This : Instance)
      return Minerva.Trees.Expressions.Class_Reference
     with Pre'Class => This.Has_Initializer;

   function Entry_Type
     (This : Class)
      return Minerva.Types.Class_Reference;

   procedure Set_Intrinsic
     (This : in out Class);

   procedure Initialize_Entry
     (This                 : in out Instance'Class;
      Declared_Name        : Minerva.Names.Minerva_Name;
      Declaration          : not null access Minerva.Trees.Instance'Class;
      Entry_Type           : access constant Minerva.Types.Class := null;
      Initializer          : access Minerva.Trees.Expressions.Instance'Class
      := null;
      Link_Name            : Minerva.Names.Minerva_Name :=
        Minerva.Names.Null_Minerva_Name;
      Is_Intrinsic         : Boolean := False;
      Is_Package_Entry     : Boolean := False;
      Is_Package_Reference : Boolean := False;
      Is_Type_Entry        : Boolean := False;
      Is_Constant          : Boolean := False;
      Is_Static            : Boolean := False;
      Is_Overloadable      : Boolean := False;
      Has_Address          : Boolean := False;
      Has_Static_Address   : Boolean := False;
      Has_Initializer      : Boolean := False);

private

   subtype Dispatch is Instance'Class;

   type Entry_Flags is
      record
         Is_Intrinsic         : Boolean := False;
         Is_Package_Entry     : Boolean := False;
         Is_Package_Reference : Boolean := False;
         Is_Type_Entry        : Boolean := False;
         Is_Constant          : Boolean := False;
         Is_Static            : Boolean := False;
         Is_Overloadable      : Boolean := False;
         Has_Address          : Boolean := False;
         Has_Static_Address   : Boolean := False;
         Has_Initializer      : Boolean := False;
      end record;

   type Tree_Reference is access all Minerva.Trees.Instance'Class;
   type Expression_Reference is
     access all Minerva.Trees.Expressions.Instance'Class;

   type Instance is
     abstract new Minerva.Names.Name_Interface with
      record
         Name               : Minerva.Names.Minerva_Name;
         Link_Name          : Minerva.Names.Minerva_Name;
         Declaration        : Tree_Reference;
         Entry_Type         : Minerva.Types.Class_Reference;
         Flags              : Entry_Flags;
         Initializer        : Expression_Reference;
      end record;

   overriding function Name
     (This : Instance)
      return Minerva.Names.Minerva_Name
   is (This.Name);

   function Entry_Value
     (This : Instance)
      return Minerva.Values.Minerva_Value
   is (raise Constraint_Error with
         "entry '" & This.Cased_Text & "' has no value");

   function Is_Package_Reference
     (This : Instance)
      return Boolean
   is (This.Flags.Is_Package_Reference);

end Minerva.Entries;
