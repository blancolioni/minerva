with Minerva.Types.Callable;

package body Minerva.Trees.Expressions.Calls is

   type Call_Partial_Instance is
     new Minerva.Partials.Instance with
      record
         Actuals : Actual_Argument_Vectors.Vector;
         Call    : Expressions.Class_Reference;
         Offset  : Natural;
      end record;

   overriding procedure Push
     (This : Call_Partial_Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding procedure Pop
     (This : Call_Partial_Instance;
      Code : in out Tagatha.Code.Instance'Class);

   --------------
   -- Children --
   --------------

   overriding function Children (This : Instance) return Class_Reference_Array
   is
   begin
      return Result : Class_Reference_Array (1 .. This.Actuals.Last_Index + 1)
      do
         Result (1) := Trees.Class_Reference (This.Call);
         for I in 1 .. This.Actuals.Last_Index loop
            Result (I + 1) :=
              Trees.Class_Reference (This.Actuals.Element (I));
         end loop;
      end return;
   end Children;

   ---------------------
   -- Constrain_Types --
   ---------------------

   overriding function Constrain_Types
     (This           : in out Instance;
      Possible_Types : Minerva.Types.Lists.List;
      Environment    : Minerva.Environment.Environment_Id)
      return Minerva.Types.Lists.List
   is
      use Minerva.Types.Callable;
      subtype Call_Reference is
        Minerva.Types.Callable.Class_Reference;

      Call_Types : Minerva.Types.Lists.List;
      Arg_Types  : array (1 .. This.Actuals.Last_Index)
        of Minerva.Types.Lists.List;

      procedure Check_Call_Type
        (Call_Type   : Call_Reference;
         Return_Type : Minerva.Types.Class_Reference);

      function Match
        (Call_Type : Call_Reference)
         return Boolean;

      ---------------------
      -- Check_Call_Type --
      ---------------------

      procedure Check_Call_Type
        (Call_Type   : Call_Reference;
         Return_Type : Minerva.Types.Class_Reference)
      is
      begin
         This.Log
           ("checking call type " & Call_Type.Short_Name
            & ": convertible to " & Return_Type.Short_Name
            & " = "
            & Boolean'Image
              (Call_Type.Return_Type.Is_Convertible_To (Return_Type)));

         if Call_Type.Has_Return_Type
           and then Call_Type.Return_Type.Is_Convertible_To (Return_Type)
           and then Call_Type.Argument_Count = This.Actuals.Last_Index
         then
            for I in 1 .. Call_Type.Argument_Count loop
               Arg_Types (I).Append (Call_Type.Argument (I).Entry_Type);
            end loop;
            Call_Types.Append (Types.Class_Reference (Call_Type));
         end if;
      end Check_Call_Type;

      -----------
      -- Match --
      -----------

      function Match
        (Call_Type : Call_Reference)
         return Boolean
      is
      begin
         for I in 1 .. Call_Type.Argument_Count loop
            if not Is_Compatible (Call_Type.Argument (I).Entry_Type,
                                  Arg_Types (I))
            then
               This.Log ("match " & Call_Type.Short_Name
                         & ": failed at argument" & I'Image);
               return False;
            end if;
         end loop;
         This.Log ("match " & Call_Type.Short_Name
                   & ": success");
         return True;
      end Match;

      Found_Types : constant Minerva.Types.Lists.List :=
                      This.Call.Constrain_Types
                        (Minerva.Types.Lists.Empty_List,
                         Environment);
   begin
      This.Log
        ("constraining type");
      for Possible_Type of Possible_Types loop
         This.Log
           ("possible type: " & Possible_Type.Short_Name);
         for Call_Type of Found_Types loop
            Check_Call_Type (To_Callable (Call_Type), Possible_Type);
         end loop;
      end loop;

      for I in 1 .. This.Actuals.Last_Index loop
         declare
            Actual : Minerva.Trees.Expressions.Class_Reference renames
                       This.Actuals (I);
         begin
            Arg_Types (I) :=
              Actual.Constrain_Types (Arg_Types (I), Environment);
         end;
      end loop;

      declare
         Matching_Types : Minerva.Types.Lists.List;
      begin
         for Call_Type of Call_Types loop
            if Match (To_Callable (Call_Type)) then
               Matching_Types.Append
                 (To_Callable (Call_Type).Return_Type);
               This.Call_Types.Append (Call_Type);
            end if;
         end loop;
         return Matching_Types;
      end;
   end Constrain_Types;

   ----------------------------
   -- Create_Call_Expression --
   ----------------------------

   function Create_Call_Expression
     (Position  : GCS.Positions.File_Position;
      Call      : not null Expressions.Class_Reference;
      Arguments : Actual_Argument_Array)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with Call => Call, Actuals => <>, others => <>)
      do
         Result.Initialize (Position);
         for Arg of Arguments loop
            Result.Actuals.Append (Arg);
         end loop;
      end return;
   end Create_Call_Expression;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (This        : Instance;
      Environment : Minerva.Ids.Environment_Id)
      return Minerva.Values.Minerva_Value
   is
   begin
      return Minerva.Values.Operate
        (Operator => This.Operator,
         Left     => This.Actuals.Element (1).Evaluate (Environment),
         Right    => This.Actuals.Element (2).Evaluate (Environment));
   end Evaluate;

   overriding function Get_Partial
     (This : Instance)
      return Minerva.Partials.Reference
   is
   begin
      return new Call_Partial_Instance'
        (Minerva.Partials.Instance with
           Actuals => This.Actuals,
         Call    => This.Call,
         Offset  => 0);
   end Get_Partial;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      return "call " & This.Call.Image;
   end Image;

   ---------------
   -- Is_Static --
   ---------------

   overriding function Is_Static
     (This        : Instance;
      Environment : Minerva.Ids.Environment_Id)
      return Boolean
   is
   begin
      return This.Intrinsic
        and then (for all Actual of This.Actuals =>
                    Actual.Is_Static (Environment));
   end Is_Static;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Instance; Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      raise Constraint_Error with "cannot pop a call";
   end Pop;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Call_Partial_Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      raise Constraint_Error with "cannot pop a call";
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Call_Partial_Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      for Arg of This.Actuals loop
         Arg.Push (Code);
      end loop;
      This.Call.Push (Code);
      if This.Offset /= 0 then
         Code.Push_Constant (Tagatha.Int_32 (This.Offset));
         Code.Operate (Tagatha.Op_Add);
      end if;
   end Push;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class)
   is
   begin
      for Arg of This.Actuals loop
         Arg.Push (Code);
      end loop;
      This.Call.Push (Code);
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
        "cannot push address of call";
   end Push_Address;

   --------------
   -- Set_Type --
   --------------

   overriding procedure Set_Type
     (This          : in out Instance;
      Possible_Type : not null Minerva.Types.Class_Reference)
   is
      Found : Boolean := False;
   begin
      for Call_Type of This.Call_Types loop
         declare
            use Minerva.Types.Callable;
            Callable : constant Minerva.Types.Callable.Class_Reference :=
                         To_Callable (Call_Type);
         begin
            if Callable.Return_Type.Is_Convertible_To (Possible_Type) then
               if Found then
                  This.Add_Error ("ambiguous");
               else
                  Found := True;
                  This.Call.Set_Type (Call_Type);
                  for I in 1 .. This.Actuals.Last_Index loop
                     declare
                        Actual : constant Expressions.Class_Reference :=
                                   This.Actuals (I);
                     begin
                        Actual.Set_Type
                          (Callable.Argument (I).Entry_Type);
                     end;
                  end loop;
               end if;
            end if;
         end;
      end loop;
   end Set_Type;

end Minerva.Trees.Expressions.Calls;
