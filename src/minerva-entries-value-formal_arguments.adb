package body Minerva.Entries.Value.Formal_Arguments is

   ------------
   -- Create --
   ------------

   function Create
     (Declaration    : not null access Minerva.Trees.Class;
      Argument_Name  : Minerva.Names.Minerva_Name;
      Argument_Type  : Minerva.Types.Class_Reference;
      Mode           : Argument_Mode := In_Mode;
      Is_Aliased     : Boolean := False;
      Null_Exclusion : Boolean := False;
      Offset         : Tagatha.Argument_Index)
      return Constant_Class_Reference
   is
      This : Instance := Instance'
        (Parent with
         Mode           => Mode,
         Is_Aliased     => Is_Aliased,
         Null_Exclusion => Null_Exclusion,
         Frame_Offset   => Offset);
   begin
      Initialize_Value_Entry
        (This               => This,
         Declared_Name      => Argument_Name,
         Declaration        => Declaration,
         Entry_Type         => Argument_Type);
      return new Instance'(This);
   end Create;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      Code.Pop_Argument (This.Frame_Offset);
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      Code.Push_Argument (This.Frame_Offset);
   end Push;

   ------------------
   -- Push_Address --
   ------------------

   overriding procedure Push_Address
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      raise Constraint_Error with
        "cannot take address of argument "
        & Minerva.Names.Cased_Text (This.Name);
   end Push_Address;

end Minerva.Entries.Value.Formal_Arguments;
