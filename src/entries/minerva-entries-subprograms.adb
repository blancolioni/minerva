with Tagatha.Operands;

with Minerva.Names;

package body Minerva.Entries.Subprograms is

   procedure Add_Formal_Argument
     (This     : in out Class;
      Argument : not null Minerva.Entries.Value.Formal_Arguments
      .Constant_Class_Reference);

   procedure Push_Intrinsic
     (This : Class;
      Unit : in out Tagatha.Code.Instance);

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
      Environment_Name : Minerva.Names.Minerva_Name;
      Call_Type        : not null Minerva.Types.Callable.Class_Reference)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance do
         Result.Initialize_Subprogram
           (Declaration      => Declaration,
            Environment_Name => Environment_Name,
            Name             => Subprogram_Name,
            Link_Name        => Subprogram_Name,
            Call_Type        => Call_Type);
      end return;
   end Create;

   ------------------------------
   -- Create_Operator_Function --
   ------------------------------

   function Create_Operator_Function
     (Declaration      : not null access Minerva.Trees.Class;
      Operator         : Minerva.Operators.Minerva_Operator;
      Environment_Name : Minerva.Names.Minerva_Name;
      Call_Type        : not null Minerva.Types.Callable.Class_Reference)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance do
         Result.Initialize_Subprogram
           (Declaration      => Declaration,
            Environment_Name => Environment_Name,
            Name             => Minerva.Operators.Get_Name (Operator),
            Link_Name        => Minerva.Names.To_Name (Operator'Image),
            Call_Type        => Call_Type);
         Result.Operator := Operator;
      end return;
   end Create_Operator_Function;

   ---------------------------
   -- Initialize_Subprogram --
   ---------------------------

   procedure Initialize_Subprogram
     (This             : in out Class;
      Declaration      : not null access Minerva.Trees.Class;
      Environment_Name : Minerva.Names.Minerva_Name;
      Name             : Minerva.Names.Minerva_Name;
      Link_Name        : Minerva.Names.Minerva_Name;
      Call_Type        : not null Minerva.Types.Callable.Class_Reference)
   is
   begin
      This.Initialize_Entry
        (Declared_Name   => Name,
         Link_Name       => Minerva.Names.Join (Environment_Name, Link_Name),
         Declaration     => Declaration,
         Entry_Type      => Call_Type,
         Is_Overloadable => True);
      for I in 1 .. Call_Type.Argument_Count loop
         Add_Formal_Argument (This, Call_Type.Argument (I));
      end loop;
   end Initialize_Subprogram;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance;
      Unit : in out Tagatha.Code.Instance)
   is
   begin
      if Dispatch (This).Is_Intrinsic then
         Push_Intrinsic (This, Unit);
      else
         declare
            use Tagatha.Operands;
            Call_Type  : Minerva.Types.Callable.Class renames
                           Minerva.Types.Callable.Class (This.Entry_Type.all);
            Has_Result : constant Boolean := Call_Type.Has_Return_Type;
            Result_Type : constant Minerva.Types.Class_Reference :=
                            (if Has_Result
                             then Call_Type.Return_Type
                             else null);
            Result_Size : constant Natural :=
                            (if Has_Result
                             then Result_Type.Size_Words
                             else 0);
            Result_Operand : constant Operand_Type :=
                               (if Has_Result
                                then Set_Data_Type
                                  (Result_Type.Data_Type,
                                   Set_Size
                                     (Result_Type.Size,
                                      Stack_Operand))
                                else No_Operand);
            Arg_Count   : constant Natural :=
                            Natural (This.Argument_List.Length);
         begin
            Unit.Reserve (Result_Size);
            Unit.Push
              (Tagatha.Operands.External_Operand (Link_Name (This)));
            Unit.Call
              (Result         => Result_Operand,
               Argument_Count => Arg_Count);
            --  Unit.Skip_Next_Store;
         end;

      end if;
   end Push;

   ------------------
   -- Push_Address --
   ------------------

   overriding procedure Push_Address
     (This : Instance; Unit : in out Tagatha.Code.Instance)
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
      Unit : in out Tagatha.Code.Instance)
   is
      use all type Minerva.Operators.Minerva_Operator;
   begin
      case This.Operator is
         when Op_None =>
            raise Constraint_Error with
            This.Cased_Text & ": unlikely to be intrinsic";
         when Op_Add =>
            Unit.Operate
              (Operator => Tagatha.Op_Add);
         when Op_Subtract =>
            Unit.Operate
              (Operator => Tagatha.Op_Sub);
         when Op_EQ =>
            Unit.Operate
              (Operator => Tagatha.Op_Equal);
         when Op_NE =>
            Unit.Operate
              (Operator => Tagatha.Op_Not_Equal);
         when Op_GT =>
            Unit.Operate
              (Operator => Tagatha.Op_Greater);
         when Op_LT =>
            Unit.Operate
              (Operator => Tagatha.Op_Less);
         when Op_GE =>
            Unit.Operate
              (Operator => Tagatha.Op_Greater_Equal);
         when Op_LE =>
            Unit.Operate
              (Operator => Tagatha.Op_Less_Equal);
         when others =>
            raise Constraint_Error with
            This.Cased_Text & ": unimplemented intrinsic";
      end case;
   end Push_Intrinsic;

end Minerva.Entries.Subprograms;
