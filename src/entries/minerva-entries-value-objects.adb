package body Minerva.Entries.Value.Objects is

   ---------------------------------
   -- Create_With_Dynamic_Address --
   ---------------------------------

   function Create_With_Dynamic_Address
     (Declaration : not null access Minerva.Trees.Class;
      Object_Name : Minerva.Names.Minerva_Name;
      Object_Type : Minerva.Types.Class_Reference;
      Offset      : Tagatha.Local_Offset;
      Address     : Minerva.Trees.Expressions.Class_Reference)
      return Class_Reference
   is
   begin
      return This : constant Class_Reference := new Instance'
        (Parent with
           Address        => Address,
         Static_Address => 0,
         Local          => Offset,
         Variable_State => new Boolean'(False))
      do
         This.Initialize_Value_Entry
           (Declared_Name      => Object_Name,
            Declaration        => Declaration,
            Entry_Type         => Object_Type,
            Has_Address        => True,
            Has_Static_Address => False);
      end return;
   end Create_With_Dynamic_Address;

   ------------------------------
   -- Create_With_Local_Offset --
   ------------------------------

   function Create_With_Local_Offset
     (Declaration : not null access Minerva.Trees.Class;
      Object_Name : Minerva.Names.Minerva_Name;
      Object_Type : Minerva.Types.Class_Reference;
      Offset      : Tagatha.Local_Offset)
      return Class_Reference
   is
   begin
      return This : constant Class_Reference := new Instance'
        (Parent with
           Address          => null,
         Static_Address   => 0,
         Local            => Offset,
         Variable_State   => null)
      do
         This.Initialize_Value_Entry
           (Declared_Name      => Object_Name,
            Declaration        => Declaration,
            Entry_Type         => Object_Type);
      end return;
   end Create_With_Local_Offset;

   --------------------------------
   -- Create_With_Static_Address --
   --------------------------------

   function Create_With_Static_Address
     (Declaration : not null access Minerva.Trees.Class;
      Object_Name : Minerva.Names.Minerva_Name;
      Object_Type : Minerva.Types.Class_Reference;
      Address     : Minerva.Target.Target_Address_Type)
      return Class_Reference
   is
   begin
      return This : constant Class_Reference := new Instance'
        (Parent with
           Address        => null,
         Static_Address => Address,
         Local            => 1,
         Variable_State   => null)
      do
         This.Initialize_Value_Entry
           (Declared_Name      => Object_Name,
            Declaration        => Declaration,
            Entry_Type         => Object_Type,
            Has_Address        => True,
            Has_Static_Address => True);
      end return;
   end Create_With_Static_Address;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      if This.Has_Static_Address then
         Unit.Pop_Address
           (Address  => This.Static_Address'Image,
            Data     => This.Data_Type,
            Size     => This.Size);
      elsif This.Has_Address then
         if This.Variable_State.all then
            Unit.Push_Local
              (Offset => This.Local,
               Data   => Tagatha.Address_Data,
               Size   => Tagatha.Default_Address_Size);
            Unit.Store (This.Size);
         else
            Unit.Pop_Local
              (Offset => This.Local,
               Data   => This.Data_Type,
               Size   => Tagatha.Default_Address_Size);
            This.Variable_State.all := True;
         end if;
      else
         Unit.Pop_Local
           (Offset => This.Local,
            Data   => This.Data_Type,
            Size   => This.Size);
      end if;
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      if This.Has_Static_Address then
         Unit.Push_Label
           (Label_Name => This.Static_Address'Image,
            Data       => This.Data_Type,
            Size       => This.Size);
      elsif This.Has_Address then
         Unit.Push_Local
           (Offset => This.Local,
            Data   => Tagatha.Address_Data);
         Unit.Dereference
           (Data => This.Data_Type,
            Size => This.Size);
      else
         Unit.Push_Local
           (Offset => This.Local,
            Data   => This.Data_Type,
            Size   => This.Size);
      end if;
   end Push;

   ------------------
   -- Push_Address --
   ------------------

   overriding procedure Push_Address
     (This : Instance; Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      if This.Has_Static_Address then
         Unit.Push
           (Value => Tagatha.Tagatha_Integer (This.Static_Address),
            Size  => Tagatha.Default_Address_Size);
      elsif This.Has_Address then
         Unit.Push_Local
           (Offset => This.Local,
            Data   => Tagatha.Address_Data);
      else
         Unit.Push_Local_Address
           (Offset => This.Local);
      end if;
   end Push_Address;

end Minerva.Entries.Value.Objects;
