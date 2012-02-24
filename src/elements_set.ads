----------------------------------------------------------------------
--  Elements_Set - Package specification                            --
--  Copyright (C) 2012 Adalog                                       --
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

pragma Ada_05;
with
  Asis;

private with
  Binary_Map,
  Ada.Strings.Wide_Unbounded;
package Elements_Set is
   type Set is private;   -- Object semantic, initially empty
   Empty_Set : constant Set;

   procedure Add (To : in out Set; Element  : Asis.Element);
   procedure Add (To : in out Set; Elements : Asis.Element_List);

   function Elements_In_Set (S : Set) return Asis.Defining_Name_List;

   procedure Clear (The_Set : in out Set);
private
   use Ada.Strings.Wide_Unbounded;
   package Element_Map is new Binary_Map (Key_Type   => Unbounded_Wide_String,
                                          Value_Type => Asis.Element);
   type Set is new Element_Map.Map;
   Empty_Set : constant Set := Set (Element_Map.Empty_Map);
end Elements_Set;
