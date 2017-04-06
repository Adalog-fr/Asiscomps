----------------------------------------------------------------------
--  Implementation_Options.ADP_Project_File - Package body          --
--  Copyright (C) 2002-2016 Adalog                                  --
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
   Ada.Text_IO;

package body Implementation_Options.ADP_Project_File is

   function Options (Project_Name : String; Option : String; Key : String) return Wide_String is
      use Ada.Text_IO;

      F : File_Type;
      function Get_Next_Src return Wide_String is
         use Ada.Characters.Handling;

         Buf  : String (1 .. 500);
         Last : Natural;
      begin
         loop  -- Exit on End_Error
            Get_Line (F, Buf, Last);
            if Last > Key'Length and then
              Buf (1 .. Key'Length) = Key
            then
               return To_Wide_String (Option) & To_Wide_String (Buf (Key'Length + 1 .. Last)) & ' ' & Get_Next_Src;
            end if;
         end loop;
      exception
            -- It is better to catch End_Error than to check End_Of_File
            -- in the case of malformed input files
         when End_Error =>
            return "";
      end Get_Next_Src;

   begin    -- Options
      Open (F, In_File, Project_Name);
      declare
         Result : constant Wide_String := Get_Next_Src;
      begin
         Close (F);
         return Result;
      end;
   exception
      when Name_Error =>
         raise Implementation_Error with "Unknown ADP project: " & Project_Name;
      when others =>
         if Is_Open (F) then
            Close (F);
         end if;
         raise;
   end Options;

   --------------------
   -- Is_Appropriate --
   --------------------

   function Is_Appropriate (Project_Name : String) return Boolean is
      use Ada.Characters.Handling;
   begin
      return Project_Name'Length >= 5
        and then To_Upper (Project_Name (Project_Name'Last - 3 .. Project_Name'Last)) = ".ADP";
   end Is_Appropriate;

   ---------------
   -- I_Options --
   ---------------

   function I_Options (Project_Name : String) return Wide_String is
   begin
      return Options (Project_Name, Option => "-I", Key => "src_dir=");
   end I_Options;

   ---------------
   -- T_Options --
   ---------------

   function T_Options (Project_Name : String) return Wide_String is
   begin
      return Options (Project_Name, Option => "-T", Key => "obj_dir=");
   end T_Options;

end Implementation_Options.ADP_Project_File;
