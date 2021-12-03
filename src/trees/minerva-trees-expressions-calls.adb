with Minerva.Logging;

with Minerva.Types.Callable;

package body Minerva.Trees.Expressions.Calls is

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

   --------------------
   -- Constrain_Type --
   --------------------

   overriding procedure Constrain_Type
     (This           : in out Instance;
      Possible_Types : Minerva.Types.Lists.List)
   is
      use Minerva.Types.Callable;
      subtype Call_Reference is
        Minerva.Types.Callable.Class_Reference;

      Call_Types : Minerva.Types.Lists.List;
      Arg_Types : array (1 .. This.Actuals.Last_Index)
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
                                  This.Actuals.Element (I).Available_Types)
            then
               return False;
            end if;
         end loop;
         return True;
      end Match;

   begin
      Minerva.Logging.Log
        (This.Image,
         "constraining type");
      for Available of This.Available_Types loop
         Minerva.Logging.Log
           (This.Image, "available type: " & Available.Short_Name);
      end loop;

      for Possible_Type of Possible_Types loop
         Minerva.Logging.Log
           (This.Image,
            "possible type: " & Possible_Type.Short_Name);

         if Is_Compatible (Possible_Type, This.Available_Types) then
            for Call_Type of This.Call.Available_Types loop
               Check_Call_Type (To_Callable (Call_Type), Possible_Type);
            end loop;
         end if;
      end loop;

      for I in 1 .. This.Actuals.Last_Index loop
         declare
            Actual : Minerva.Trees.Expressions.Class_Reference renames
                       This.Actuals (I);
         begin
            for Arg_Type of Arg_Types (I) loop
               Actual.Log ("constrained to "
                           & Arg_Type.Short_Name);
            end loop;
            Actual.Constrain_Type (Arg_Types (I));
         end;
      end loop;

      This.Available_Types.Clear;

      declare
         Matching_Call_Types : Minerva.Types.Lists.List;
      begin
         for Call_Type of Call_Types loop
            if Match (To_Callable (Call_Type)) then
               Matching_Call_Types.Append (Call_Type);
            end if;
         end loop;

         if Matching_Call_Types.Is_Empty then
            This.Add_Error ("no-interpretation-matches");
         elsif Natural (Matching_Call_Types.Length) > 1 then
            This.Add_Error ("ambiguous-call," & This.Call.Image);
         else
            declare
               Call_Type       : constant Call_Reference :=
                                   To_Callable
                                     (Matching_Call_Types.First_Element);
               pragma Assert (Call_Type.Has_Return_Type,
                              "expected a function type");
            begin
               This.Call.Set_Type (Types.Class_Reference (Call_Type));
               This.Available_Types.Append (Call_Type.Return_Type);
               for I in 1 .. This.Actuals.Last_Index loop
                  declare
                     Actual : Minerva.Trees.Expressions.Class_Reference renames
                                This.Actuals (I);
                  begin
                     Actual.Set_Type
                       (Call_Type.Argument (I).Entry_Type);
                  end;
               end loop;
            end;
         end if;
      end;
   end Constrain_Type;

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
     (This : Instance; Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      raise Constraint_Error with "cannot pop a call";
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      for Arg of This.Actuals loop
         Arg.Push (Unit);
      end loop;
      This.Call.Push (Unit);
   end Push;

   -------------------------
   -- Set_Available_Types --
   -------------------------

   overriding procedure Set_Available_Types
     (This        : in out Instance;
      Environment : Minerva.Ids.Environment_Id)
   is
   begin
      This.Call.Set_Available_Types (Environment);
      for Actual of This.Actuals loop
         Actual.Set_Available_Types (Environment);
      end loop;
      for Available_Type of This.Call.Available_Types loop
         This.Available_Types.Append
           (Minerva.Types.Callable.To_Callable (Available_Type)
            .Return_Type);
      end loop;
      This.Constrain_Type (This.Possible_Types);
   end Set_Available_Types;

end Minerva.Trees.Expressions.Calls;
