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
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Unbounded;

package body Implementation_Options is

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

   function Parameters_String (Project       : Project_File.Class_Access := null;
                               Other_Options : Wide_String := "") return Wide_String
   is
      use Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Unbounded;
      use Project_File;

      Default_Options : Unbounded_Wide_String;
   begin
      if Index (Other_Options, "-C") = 0 then
         Default_Options := To_Unbounded_Wide_String ("-C" & Default_C_Parameter);
      end if;
      if Index (Other_Options, "-F") = 0 then
         Default_Options := Default_Options & To_Unbounded_Wide_String (" -F" & Default_F_Parameter);
      end if;

      if Project = null then  -- No project file
         return
           To_Wide_String (Default_Options)
           & ' ' & Other_Options;
      else
         return
           To_Wide_String (Default_Options)
           & ' ' & Project.I_Options
           & ' ' & Project.T_Options
           & ' ' & Other_Options;
      end if;
   end Parameters_String;

end Implementation_Options;
