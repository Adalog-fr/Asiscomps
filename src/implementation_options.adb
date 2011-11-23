----------------------------------------------------------------------
--  Implementation_Options - Package body                           --
--  Copyright (C) 2005 Adalog                                       --
--  Author: J-P. Rosen                                              --
--                                                                  --
--  ADALOG   is   providing   training,   consultancy,   expertise, --
--  assistance and custom developments  in Ada and related software --
--  engineering techniques.  For more info about our services:      --
--  ADALOG                   Tel: +33 1 41 24 31 40                 --
--  19-21 rue du 8 mai 1945  Fax: +33 1 41 24 07 36                 --
--  94110 ARCUEIL            E-m: info@adalog.fr                    --
--  FRANCE                   URL: http://www.adalog.fr              --
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
package body Implementation_Options is

   -------------------------------------------------------------
   -- Internal Elements                                       --
   -------------------------------------------------------------

   -- This functions constructs a list of -I<name> options from
   -- the src_file indications in a Gnat project file

   function I_Options_From_Project (Project_File : String) return String is
      use Ada.Text_IO;
      F : File_Type;
      Key : constant String := "src_dir=";
      function Get_Next_Src return String is
         Buf  : String (1..500);
         Last : Natural;
      begin
         loop  -- Exit on End_Error
            Get_Line (F, Buf, Last);
            if Last > Key'Length and then
              Buf (1..Key'Length) = Key
            then
               return "-I" & Buf(Key'Length+1..Last) & ' ' & Get_Next_Src;
            end if;
         end loop;
      exception
         -- It is better to catch End_Error than to check End_Of_File
         -- in the case of malformed input files
         when End_Error =>
            return "";
      end Get_Next_Src;

   begin    -- I_Options_From_Project
      if Project_File = "" then
         -- No project file
         return "";
      end if;

      Open (F, In_File, Project_File);
      declare
         Result : constant String := Get_Next_Src;
      begin
         Close (F);
         return Result;
      end;
   exception
      when others =>
         if Is_Open (F) then
            Close (F);
         end if;
         raise;
   end I_Options_From_Project;

   -------------------------------------------------------------
   -- Exported Elements                                       --
   -------------------------------------------------------------

   -----------------------
   -- Initialize_String --
   -----------------------

   function Initialize_String (Debug_Mode : Boolean := False) return Wide_String is
--      Default : constant Wide_String := "-ws -k";
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
      return
        To_Wide_String (Default_Options)
        & ' ' & To_Wide_String (I_Options_From_Project (Project_File))
        & ' ' & Other_Options
        ;
   end Parameters_String;

end Implementation_Options;

