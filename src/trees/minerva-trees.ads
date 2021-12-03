with GCS.Positions;

with Tagatha.Units;

with Minerva.Environment;
with Minerva.Ids;

with Minerva.Errors;

limited with Minerva.Entries;

private package Minerva.Trees is

   type Instance is abstract tagged private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   type Constant_Reference is access all Instance;
   type Constant_Class_Reference is access all Class;

   type Class_Reference_Array is
     array (Positive range <>) of Class_Reference;

   Null_Class_Reference_Array : Class_Reference_Array (1 .. 0);

   function Image
     (This : Instance)
      return String
      is abstract;

   function Elaborated (This : Class) return Boolean;
   function Checked (This : Class) return Boolean;

   procedure Elaborate
     (This        : not null access Class;
      Environment : Minerva.Environment.Environment_Id);

   procedure Check
     (This        : in out Class;
      Environment : Minerva.Environment.Environment_Id)
     with Pre => This.Elaborated;

   procedure Compile
     (This : Class;
      Unit : in out Tagatha.Units.Tagatha_Unit)
     with Pre => This.Elaborated and then This.Checked;

   procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id);

   procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id);

   procedure Compile_Tree
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is abstract;

   function Has_Environment
     (This : Class)
      return Boolean;

   function Get_Environment
     (This : Class)
      return Minerva.Ids.Environment_Id;

   function Has_Entry
     (This : Class)
      return Boolean;

   function Get_Entry
     (This : Class)
      return Minerva.Entries.Constant_Class_Reference;

   procedure Set_Entry
     (This       : in out Class;
      Tree_Entry : not null access constant Minerva.Entries.Instance'Class);

   function As_Tree
     (This : not null access Class)
      return Class_Reference;

   function Children
     (This : Instance)
      return Class_Reference_Array
      is abstract
     with Post'Class => (for all Child of Children'Result => Child /= null);

   function Source_Position
     (This : Class)
      return GCS.Positions.File_Position;

   function Has_Errors
     (This : Class)
      return Boolean;

   function Errors
     (This : Class)
      return Minerva.Errors.Error_Store;

   procedure Add_Error
     (This : in out Class;
      Tag  : String);

   procedure Initialize
     (This     : in out Instance;
      Position : GCS.Positions.File_Position);

   procedure Iterate_Children
     (This : Class;
      Process : not null access
        procedure (Child : not null access Class));

   procedure Iterate_All
     (This    : Class;
      Process : not null access
        procedure (Item : not null access Class));

   procedure Put
     (This : Class);

   procedure Log
     (This    : Class;
      Message : String);

private

   subtype Dispatch is Instance'Class;

   type Entry_Class_Reference is
     access constant Minerva.Entries.Instance'Class;

   type Instance is abstract tagged
      record
         Elaborated  : Boolean := False;
         Checked     : Boolean := False;
         Has_Entry   : Boolean := False;
         Position    : GCS.Positions.File_Position;
         Errors      : Minerva.Errors.Error_Store;
         Environment : Minerva.Ids.Environment_Id_Base :=
                         Minerva.Ids.Null_Environment_Id;
         Tree_Entry  : Entry_Class_Reference;
      end record;

   function As_Tree
     (This : not null access Class)
      return Class_Reference
   is (Class_Reference (This));

   function Source_Position
     (This : Class)
      return GCS.Positions.File_Position
   is (This.Position);

   function Elaborated (This : Class) return Boolean
   is (This.Elaborated);

   function Checked (This : Class) return Boolean
   is (This.Checked);

   function Maybe (Item : access Trees.Class) return Class_Reference_Array
   is (if Item = null then Null_Class_Reference_Array
       else (1 => Trees.Class_Reference (Item)));

   function Has_Entry
     (This : Class)
      return Boolean
   is (This.Has_Entry);

   function Has_Environment
     (This : Class)
      return Boolean
   is (Minerva.Ids."/=" (This.Environment, Minerva.Ids.Null_Environment_Id));

   function Get_Environment
     (This : Class)
      return Minerva.Ids.Environment_Id
   is (This.Environment);

end Minerva.Trees;
