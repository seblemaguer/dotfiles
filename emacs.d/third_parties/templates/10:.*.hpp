/*
   `(file-name-nondirectory (buffer-file-name))`

    Copyright (C) `user-full-name` <`user-mail-address`> `(format-time-string "%e %B %Y")`

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


#ifndef INCLUDED_`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H
#define INCLUDED_`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H 1

class `(capitalize (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`
{
public:
    `(capitalize (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`();
    virtual ~`(capitalize (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`();

    $0
};

#endif /* INCLUDED_`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H */


/* (>>>FILE<<<) ends here */
