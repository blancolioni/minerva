with Minerva.Names;

package body Minerva.Entries.Subprograms is

   procedure Add_Formal_Argument
     (This     : in out Class;
      Argument : not null Minerva.Entries.Value.Formal_Arguments
      .Constant_Class_Reference);

   procedure Push_Intrinsic
     (This : Class;
      Code : in out Tagatha.Code.Instance'Class);

   type Subprogram_Partial_Instance is
     new Minerva.Partials.Instance with
      record
         Subprogram : Constant_Class_Reference;
      end record;

   overriding procedure Push
     (This : Subprogram_Partial_Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding procedure Pop
     (This : Subprogram_Partial_Instance;
      Code : in out Tagatha.Code.Instance'Class);

   -------------------------
   -- Add_Formal_Argument --
   -------------------------

   procedure Add_Formal_Argument
     (This     : in out Class;
      Argument : not null Minerva.Entries.Value.Formal_Arguments
      .Constant_Class_Reference)
   is
   begin
      This.Argument_List.Append (Argument);
   end Add_Formal_Argument;

   ------------
   -- Create --
   ------------

   function Create
     (Declaration      : not null access Minerva.Trees.Class;
      Subprogram_Name  : Minerva.Names.Minerva_Name;
      Container_Name   : Minerva.Names.Minerva_Name;
      Call_Type        : not null Minerva.Types.Callable.Class_Reference)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance do
         Result.Initialize_Subprogram
           (Declaration, Subprogram_Name, Container_Name,
            Call_Type);
      end return;
   end Create;

   ------------------------------
   -- Create_Operator_Function --
   ------------------------------

   function Create_Operator_Function
     (Declaration    : not null access Minerva.Trees.Class;
      Operator       : Minerva.Operators.Minerva_Operator;
      Call_Type      : not null Minerva.Types.Callable.Class_Reference)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance do
         Result.Initialize_Subprogram
           (Declaration, Minerva.Operators.Get_Name (Operator),
            Minerva.Names.Empty_Name,
            Call_Type);
         Result.Operator := Operator;
      end return;
   end Create_Operator_Function;

   -----------------
   -- Get_Partial --
   -----------------

   overriding function Get_Partial
     (This : not null access constant Instance)
      return Minerva.Partials.Reference
   is
   begin
      return new Subprogram_Partial_Instance'
        (Minerva.Partials.Instance with
           Subprogram => Constant_Class_Reference (This));
   end Get_Partial;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize_Subprogram
     (This           : in out Class;
      Declaration    : not null access Minerva.Trees.Class;
      Name           : Minerva.Names.Minerva_Name;
      Container_Name : Minerva.Names.Minerva_Name;
      Call_Type      : not null Minerva.Types.Callable.Class_Reference)
   is
   begin
      This.Initialize_Entry
        (Declared_Name   => Name,
         Declaration     => Declaration,
         Entry_Type      => Call_Type,
         Is_Overloadable => True);
      This.Parent_Name := Container_Name;
      for I in 1 .. Call_Type.Argument_Count loop
         Add_Formal_Argument (This, Call_Type.Argument (I));
      end loop;
   end Initialize_Subprogram;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Subprogram_Partial_Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      This.Subprogram.Pop (Code);
   end Pop;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Instance; Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      raise Constraint_Error with
        "cannot pop subprogram: " & This.Cased_Text;
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Subprogram_Partial_Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      This.Subprogram.Push (Code);
   end Push;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      if Dispatch (This).Is_Intrinsic then
         Push_Intrinsic (This, Code);
      else
         Code.Call
           (Name           => This.link_name,
            Argument_Count => Natural (This.Argument_List.Length),
            Result_Count   =>
              (if Minerva.Types.Callable.Class (This.Entry_Type.all)
               .Has_Return_Type
               then 1 else 0));
      end if;
   end Push;

   ------------------
   -- Push_Address --
   ------------------

   overriding procedure Push_Address
     (This : Instance; Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      raise Constraint_Error with
        "subprogram address not implmented";
   end Push_Address;

   --------------------
   -- Push_Intrinsic --
   --------------------

   procedure Push_Intrinsic
     (This : Class;
      Code : in out Tagatha.Code.Instance'Class)
   is
      use all type Minerva.Operators.Minerva_Operator;
      Result_Type : constant Minerva.Types.Class_Reference :=
                      Minerva.Types.Callable.Class_Reference
                        (This.Entry_Type)
                        .Return_Type;
   begin
      case This.Operator is
         when Op_None =>
            raise Constraint_Error with
            This.Cased_Text & ": unlikely to be intrinsic";
         when Op_Add =>
            if Result_Type.Is_Floating_Point then
               Code.Operate (Tagatha.Op_Fadd);
            else
               Code.Operate (Tagatha.Op_Add);
            end if;

         when Op_Sub =>
            if Result_Type.Is_Floating_Point then
               Code.Operate (Tagatha.Op_Fsub);
            else
               Code.Operate (Tagatha.Op_Subtract);
            end if;

         when Op_Mul =>
            if Result_Type.Is_Floating_Point then
               Code.Operate (Tagatha.Op_Fmul);
            else
               Code.Operate (Tagatha.Op_Multiply);
            end if;

         when Op_Div =>
            if Result_Type.Is_Floating_Point then
               Code.Operate (Tagatha.Op_Fdiv);
            else
               Code.Operate (Tagatha.Op_Divide);
            end if;

         when Op_Mod =>
            Code.Operate
              (Op => Tagatha.Op_Mod);
         when Op_EQ =>
            Code.Operate
              (Op => Tagatha.Op_EQ);
         when Op_NE =>
            Code.Operate
              (Op => Tagatha.Op_NE);
         when Op_GT =>
            Code.Operate
              (Op => Tagatha.Op_GT);
         when Op_LT =>
            Code.Operate
              (Op => Tagatha.Op_LT);
         when Op_GE =>
            Code.Operate
              (Op => Tagatha.Op_GE);
         when Op_LE =>
            Code.Operate
              (Op => Tagatha.Op_LE);
         when others =>
            raise Constraint_Error with
            This.Cased_Text & ": unimplemented intrinsic";
      end case;
   end Push_Intrinsic;

end Minerva.Entries.Subprograms;
