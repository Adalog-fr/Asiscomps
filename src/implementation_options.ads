----------------------------------------------------------------------
--  Implementation_Options - Package specification                  --
--  Copyright (C) 2002 Adalog                                       --
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

with   -- Reusable components
   Project_File;

package Implementation_Options is
   -- This package defines parameter strings that are implementation dependent.
   -- There is no other (known) dependency on the ASIS implentation

   -- Parameters for ASIS
   function Initialize_String (Debug_Mode : Boolean := False) return Wide_String;
   function Parameters_String (Project       : Project_File.Class_Access := null;
                               Other_Options : Wide_String := "") return Wide_String;
   Default_C_Parameter : Wide_Character := 'A';   -- -CA
   Default_F_Parameter : Wide_Character := 'M';   -- -FM


   -- Parameters for Wide_Text_IO:
   Form_Parameters : constant String := "WCEM=h"; -- Use Hex encoding


   -- Implementation dependant information
   function Tree_Generator return Wide_String;
   -- Full path to Gnat tree generator (gcc or asis-gcc)


   Implementation_Error : exception;
end Implementation_Options;
