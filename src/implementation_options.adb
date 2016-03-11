----------------------------------------------------------------------
--  Implementation_Options - Package body                           --
--  Copyright (C) 2005-2016 Adalog                                  --
--  Author: J-P. Rosen                                              --
--                                                                  --
--  ADALOG   is   providing   training,   consultancy,   expertise, --
--  assistance and custom developments  in Ada and related software --
--  engineering techniques.  For more info about our services:      --
--  ADALOG                          Tel: +33 1 45 29 21 52          --
--  2 rue du Docteur Lombard        Fax: +33 1 45 29 25 00          --
--  92441 ISSY LES MOULINEAUX CEDEX E-m: info@adalog.fr             --
--  FRANCE                          URL: http://www.adalog.fr       --
--                                                                  --
--  This  unit is  free software;  you can  redistribute  it and/or --
--  modify  it under  terms of  the GNU  General Public  License as --
--  published by the Free Software Foundation; either version 2, or --
--  (at your  option) any later version.  This  unit is distributed --
--  in the hope  that it will be useful,  but WITHOUT ANY WARRANTY; --
--  without even the implied warranty of MERCHANTABILITY or FITNESS --
--  FOR A  PARTICULAR PURPOSE.  See the GNU  General Public License --
--  for more details.   You should have received a  copy of the GNU --
--  General Public License distributed  with this program; see file --
--  COPYING.   If not, write  to the  Free Software  Foundation, 59 --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.           --
--                                                                  --
--  As  a special  exception, if  other files  instantiate generics --
--  from  this unit,  or you  link this  unit with  other  files to --
--  produce an executable,  this unit does not by  itself cause the --
--  resulting executable  to be covered  by the GNU  General Public --
--  License.  This exception does  not however invalidate any other --
--  reasons why  the executable  file might be  covered by  the GNU --
--  Public License.                                                 --
----------------------------------------------------------------------
with -- Standard Ada units
  Ada.Characters.Handling,
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Unbounded,
  Ada.Text_IO;

with -- GNAT units
  Gnatcoll.Projects,
  Gnatcoll.VFS;

package body Implementation_Options is

   -------------------------------------------------------------
   -- Internal Elements                                       --
   -------------------------------------------------------------

   -- This functions constructs a list of -I<name> options from
   -- the src_file indications in an Emacs .adp project file

   function I_Options_From_ADP_Project (Project_File : String) return Wide_String is
      use Ada.Text_IO;

      F : File_Type;
      Key : constant String := "src_dir=";
      function Get_Next_Src return Wide_String is
         use Ada.Characters.Handling;

         Buf  : String (1..500);
         Last : Natural;
      begin
         loop  -- Exit on End_Error
            Get_Line (F, Buf, Last);
            if Last > Key'Length and then
              Buf (1..Key'Length) = Key
            then
               return "-I" & To_Wide_String (Buf(Key'Length+1..Last)) & ' ' & Get_Next_Src;
            end if;
         end loop;
      exception
         -- It is better to catch End_Error than to check End_Of_File
         -- in the case of malformed input files
         when End_Error =>
            return "";
      end Get_Next_Src;

   begin    -- I_Options_From_ADP_Project
      Open (F, In_File, Project_File);
      declare
         Result : constant Wide_String := Get_Next_Src;
      begin
         Close (F);
         return Result;
      end;
   exception
      when Name_Error =>
         raise Implementation_Error with "Unknown ADP project: " & Project_File;
      when others =>
         if Is_Open (F) then
            Close (F);
         end if;
         raise;
   end I_Options_From_ADP_Project;

   -- This functions constructs a list of -I<name> options from
   -- the source_dirs indications in a Gnat .gpr project file

   function I_Options_From_GPR_Project (Project_File : String) return Wide_String is
      use Ada.Characters.Handling, Ada.Strings.Wide_Unbounded;
      use Gnatcoll.Projects, Gnatcoll.VFS;

      Tree   : Project_Tree;
      Result : Unbounded_Wide_String;
   begin    -- I_Options_From_GPR_Project
      Load (Tree, Root_Project_Path => Create (+Project_File));

      declare
         Project_Dirs : constant File_Array := Source_Dirs (Root_Project (Tree), Recursive => True);
      begin
         for D in Project_Dirs'Range loop
            Append (Result, " -I" & To_Wide_String (+Full_Name (Project_Dirs (D))));
         end loop;
      end;

      return To_Wide_String (Result);

   exception
      when Invalid_Project =>
         raise Implementation_Error with "Unknown or invalid GPR project: " & Project_File;
   end I_Options_From_GPR_Project;


   -------------------------------------------------------------
   -- Exported Elements                                       --
   -------------------------------------------------------------

   -----------------------
   -- Initialize_String --
   -----------------------

   function Initialize_String (Debug_Mode : Boolean := False) return Wide_String is
      Default : constant Wide_String := "-ws -k -asis05";
   begin
      if Debug_Mode then
         return Default;
      else
         return Default & " -nbb";   -- No Bug Box
      end if;
   end Initialize_String;

  -----------------------
   -- Parameters_String --
   -----------------------

   function Parameters_String (Project_File  : String := "";
                               Other_Options : Wide_String := "") return Wide_String
   is
      use Ada.Characters.Handling, Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Unbounded;
      Default_Options : Unbounded_Wide_String;
   begin
      if Index (Other_Options, "-C") = 0 then
         Default_Options := To_Unbounded_Wide_String ("-C" & Default_C_Parameter);
      end if;
      if Index (Other_Options, "-F") = 0 then
         Default_Options := Default_Options & To_Unbounded_Wide_String (" -F" & Default_F_Parameter);
      end if;

      if Project_File = "" then  -- No project file
         return
           To_Wide_String (Default_Options)
           & ' ' & Other_Options;
      elsif Project_File'Length < 5 then
         raise Implementation_Error with "Incorrect project file: " & Project_File;
      elsif To_Upper (Project_File (Project_File'Last - 3 .. Project_File'Last)) = ".GPR" then
         return
           To_Wide_String (Default_Options)
           & ' ' & I_Options_From_GPR_Project (Project_File)
           & ' ' & Other_Options;
      elsif To_Upper (Project_File (Project_File'Last - 3 .. Project_File'Last)) = ".ADP" then
         return
           To_Wide_String (Default_Options)
           & ' ' & I_Options_From_ADP_Project (Project_File)
           & ' ' & Other_Options;
      else
         raise Implementation_Error with "Incorrect project file: " & Project_File;
      end if;

   end Parameters_String;

end Implementation_Options;
