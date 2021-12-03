with Minerva.Names;

package body Minerva.Entries.Subprograms is

   procedure Add_Formal_Argument
     (This     : in out Class;
      Argument : not null Minerva.Entries.Value.Formal_Arguments
      .Constant_Class_Reference);

   procedure Push_Intrinsic
     (This : Class;
      Unit : in out Tagatha.Units.Tagatha_Unit);

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
      Call_Type        : not null Minerva.Types.Callable.Class_Reference)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance do
         Result.Initialize_Subprogram
           (Declaration, Subprogram_Name,
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
           (Declaration, Minerva.Operators.Get_Name (Operator), Call_Type);
         Result.Operator := Operator;
      end return;
   end Create_Operator_Function;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize_Subprogram
     (This        : in out Class;
      Declaration : not null access Minerva.Trees.Class;
      Name        : Minerva.Names.Minerva_Name;
      Call_Type   : not null Minerva.Types.Callable.Class_Reference)
   is
   begin
      This.Initialize_Entry
        (Declared_Name   => Name,
         Declaration     => Declaration,
         Entry_Type      => Call_Type,
         Is_Overloadable => True);
      for I in 1 .. Call_Type.Argument_Count loop
         Add_Formal_Argument (This, Call_Type.Argument (I));
      end loop;
   end Initialize_Subprogram;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Instance; Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      raise Constraint_Error with
        "cannot pop subprogram: " & This.Cased_Text;
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      if Dispatch (This).Is_Intrinsic then
         Push_Intrinsic (This, Unit);
      else
         Unit.Call (This.Link_Name, Natural (This.Argument_List.Length));
      end if;
   end Push;

   ------------------
   -- Push_Address --
   ------------------

   overriding procedure Push_Address
     (This : Instance; Unit : in out Tagatha.Units.Tagatha_Unit)
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
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
      use all type Minerva.Operators.Minerva_Operator;
   begin
      case This.Operator is
         when Op_None =>
            raise Constraint_Error with
            This.Cased_Text & ": unlikely to be intrinsic";
         when Op_Add =>
            Unit.Operate
              (Op => Tagatha.Op_Add);
         when Op_Subtract =>
            Unit.Operate
              (Op => Tagatha.Op_Sub);
         when Op_EQ =>
            Unit.Operate
              (Op => Tagatha.Op_Equal);
         when Op_NE =>
            Unit.Operate
              (Op => Tagatha.Op_Not_Equal);
         when Op_GT =>
            Unit.Operate
              (Op => Tagatha.Op_Greater);
         when Op_LT =>
            Unit.Operate
              (Op => Tagatha.Op_Less);
         when Op_GE =>
            Unit.Operate
              (Op => Tagatha.Op_Greater_Equal);
         when Op_LE =>
            Unit.Operate
              (Op => Tagatha.Op_Less_Equal);
         when others =>
            raise Constraint_Error with
            This.Cased_Text & ": unimplemented intrinsic";
      end case;
   end Push_Intrinsic;

end Minerva.Entries.Subprograms;
