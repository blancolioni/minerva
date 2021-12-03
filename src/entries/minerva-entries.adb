with Minerva.Trees.Expressions;

package body Minerva.Entries is

   -----------------
   -- Declaration --
   -----------------

   function Declaration (This : Class) return Minerva.Trees.Class_Reference is
   begin
      return Minerva.Trees.Class_Reference (This.Declaration);
   end Declaration;

   ----------------
   -- Entry_Type --
   ----------------

   function Entry_Type (This : Class) return Minerva.Types.Class_Reference is
   begin
      return This.Entry_Type;
   end Entry_Type;

   -----------------
   -- Has_Address --
   -----------------

   function Has_Address (This : Class) return Boolean is
   begin
      return This.Flags.Has_Address;
   end Has_Address;

   ---------------------
   -- Has_Initializer --
   ---------------------

   function Has_Initializer (This : Class) return Boolean is
   begin
      return This.Flags.Has_Initializer;
   end Has_Initializer;

   ------------------------
   -- Has_Static_Address --
   ------------------------

   function Has_Static_Address (This : Instance) return Boolean is
   begin
      return This.Flags.Has_Static_Address;
   end Has_Static_Address;

   ----------------------
   -- Initialize_Entry --
   ----------------------

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
      Has_Initializer      : Boolean := False)
   is
   begin
      This.Flags :=
        Entry_Flags'
          (Is_Intrinsic         => Is_Intrinsic,
           Is_Type_Entry        => Is_Type_Entry,
           Is_Package_Entry     => Is_Package_Entry,
           Is_Package_Reference => Is_Package_Reference,
           Is_Constant          => Is_Constant,
           Is_Static            => Is_Static,
           Is_Overloadable      => Is_Overloadable,
           Has_Address          => Has_Address,
           Has_Static_Address   => Has_Static_Address,
           Has_Initializer      => Has_Initializer);
      This.Name := Declared_Name;
      This.Declaration := Tree_Reference (Declaration);
      This.Entry_Type := Minerva.Types.Class_Reference (Entry_Type);
      This.Initializer := Expression_Reference (Initializer);
   end Initialize_Entry;

   -----------------
   -- Initializer --
   -----------------

   function Initializer
     (This : Instance)
      return Minerva.Trees.Expressions.Class_Reference
   is
   begin
      return Minerva.Trees.Expressions.Class_Reference (This.Initializer);
   end Initializer;

   -----------------
   -- Is_Constant --
   -----------------

   function Is_Constant (This : Class) return Boolean is
   begin
      return This.Flags.Is_Constant;
   end Is_Constant;

   ------------------
   -- Is_Intrinsic --
   ------------------

   function Is_Intrinsic (This : Class) return Boolean is
   begin
      return This.Flags.Is_Intrinsic;
   end Is_Intrinsic;

   ---------------------
   -- Is_Object_Entry --
   ---------------------

   function Is_Object_Entry
     (This : Instance)
      return Boolean
   is
   begin
      return False;
   end Is_Object_Entry;

   ---------------------
   -- Is_Overloadable --
   ---------------------

   function Is_Overloadable (This : Class) return Boolean is
   begin
      return This.Flags.Is_Overloadable;
   end Is_Overloadable;

   ---------------
   -- Is_Static --
   ---------------

   function Is_Static (This : Class) return Boolean is
   begin
      return This.Flags.Is_Static;
   end Is_Static;

   -------------------
   -- Is_Type_Entry --
   -------------------

   function Is_Type_Entry (This : Class) return Boolean is
   begin
      return This.Flags.Is_Type_Entry;
   end Is_Type_Entry;

   ---------------
   -- Link_Name --
   ---------------

   function Link_Name (This : Class) return String is
   begin
      return Minerva.Names.To_String
        (Name       => This.Name,
         Separator  => "__",
         Lower_Case => True);
   end Link_Name;

   -------------------
   -- Set_Intrinsic --
   -------------------

   procedure Set_Intrinsic (This : in out Class) is
   begin
      This.Flags.Is_Intrinsic := True;
   end Set_Intrinsic;

   --------------------
   -- Static_Address --
   --------------------

   function Static_Address
     (This : Instance)
      return Minerva.Target.Target_Address_Type
   is
   begin
      return (raise Constraint_Error with
                This.Cased_Text & ": no static address");
   end Static_Address;

end Minerva.Entries;
