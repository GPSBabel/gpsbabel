#include "src/core/angle.h"
#include <iostream>
#include <ostream>

int main(void) {
	const Radians r(90);
	const Degrees d(90);
	Angle a;
//	a.FromRadians(20);
	Angle d98 = 98_deg;
	Angle d981 = 98.1_deg;
	Angle r18 = 18_rad;
	Angle r181 = 18.1_rad;
	std::cout << d98.toDegrees();
//	static_assert(r == d);
//	std::cout << Angle(r);
//	std::cout << dd.toDegrees();
}
