with Ada.Command_Line;                 use Ada.Command_Line;

with Minerva.Build;
with Minerva.Logging;
with Minerva.Options;

procedure Minerva.Driver is
begin

   if not Minerva.Options.Load then
      Ada.Command_Line.Set_Exit_Status (2);
      return;
   end if;

   Minerva.Logging.Start_Logging;

   declare
      Success : Boolean;

      procedure Process (Argument : String);

      -------------
      -- Process --
      -------------

      procedure Process (Argument : String) is
      begin
         Minerva.Logging.Log ("building: " & Argument);
         Minerva.Build.Run_Build
           (Source_Path => Argument,
            Success     => Success);
      end Process;

   begin
      Minerva.Options.Iterate_Tail_Arguments (Process'Access);

      if not Success then
         Ada.Command_Line.Set_Exit_Status (1);
      end if;

   end;

   Minerva.Logging.Stop_Logging;

exception

   when others =>
      Ada.Command_Line.Set_Exit_Status (1);
      Minerva.Logging.Stop_Logging;
      raise;
end Minerva.Driver;
