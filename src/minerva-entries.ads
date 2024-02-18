with Tagatha.Code;

with Minerva.Names;
with Minerva.Partials;

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

   function Get_Partial
     (This : not null access constant Instance)
      return Minerva.Partials.Reference
      is abstract;

   procedure Pop
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is abstract;

   procedure Push
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is abstract;

   procedure Push_Address
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
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

   type Expression_Class_Reference is
     access all Minerva.Trees.Expressions.Instance'Class;

   function Initializer
     (This : Instance)
      return Expression_Class_Reference
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
         Parent_Name        : Minerva.Names.Minerva_Name;
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
