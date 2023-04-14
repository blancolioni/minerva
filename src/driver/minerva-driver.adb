with Ada.Command_Line;                 use Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

with WL.Command_Line;

with Minerva.Build;
with Minerva.Logging;
with Minerva.Options;
with Minerva.Paths;

procedure Minerva.Driver is
   Success : Boolean;
begin
   if not Ada.Directories.Exists (".minerva-options") then
      Ada.Directories.Copy_File
        (Source_Name => Minerva.Paths.Config_File ("default-options.txt"),
         Target_Name => ".minerva-options");
   end if;

   WL.Command_Line.Load_Defaults (".minerva-options");

   Minerva.Logging.Start_Logging;

   if WL.Command_Line.Argument_Count = 0 then
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "minerva: no input files");
      Ada.Command_Line.Set_Exit_Status (2);
   else
      for I in 1 .. WL.Command_Line.Argument_Count loop
         Minerva.Logging.Log ("building: " & WL.Command_Line.Argument (I));
         Minerva.Build.Run_Build
           (Source_Path => WL.Command_Line.Argument (I),
            Success     => Success);
         if not Success then
            Ada.Command_Line.Set_Exit_Status (1);
         end if;

         if I = 1 and then not Minerva.Options.Compile_Only then
            Minerva.Build.Build_Main (WL.Command_Line.Argument (I));
         end if;
      end loop;
   end if;

   Minerva.Logging.Stop_Logging;

exception

   when others =>
      Ada.Command_Line.Set_Exit_Status (1);
      Minerva.Logging.Stop_Logging;
      raise;
end Minerva.Driver;
