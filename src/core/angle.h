
#include <numbers>
#include <iostream>
#include <ostream>

constexpr double kTau = std::numbers::pi * 2;

struct Degrees;
struct Radians;


struct Radians {
 public:
  explicit Radians(double a) : angle_(a) {}
//  Radians(Degrees a)        : angle_(a * std::numbers::pi/180.f) {}
  operator double()          { return angle_; }
 private:
  double angle_;
};

struct Degrees {
 public:
  explicit Degrees(double a) : angle_(a) {}
//  Degrees(Radians a)        : angle_(a * 180.f/std::numbers::pi) {}
  operator double()          { return angle_; }
 private:
  double angle_;
};

struct Angle {
 public:
  explicit operator double() const;

  //static constexpr Angle FromRadians(double radians);
  //static constexpr Angle FromDegrees(double degree);
  double toRadians() const;
  double toDegrees() const;
 private:
  double data_{0.f};
};


// Now, down in a class file...
double Angle::toDegrees() const
{
  return data_ / kTau * 360;
}

double Angle::toRadians() const
{
  return data_;
}

#if 0
constexpr Angle Angle::FromDegrees(double degrees)
{
	return Angle(degrees * kTau / 360);
}

constexpr Angle Angle::FromRadians(float radians)
{
	return Angle(radians);
}
#endif

#if 0
std::ostream& operator<<(std::ostream& stream, const Angle a)
{
	return stream << (a.toDegree()) << "°";
};
#endif


#if 0
constexpr Angle Angle::FromDegrees(double degrees)
{
	return Angle(degrees * kTau / 360.0);
}
#endif

/*constexpr */ Angle operator "" _deg(long double num){
	//return Angle::FromDegrees(num);
	std::cout << "Constructing DEG from double " << num << std::endl;
	return Angle();
};
/*constexpr */ Angle operator "" _deg(unsigned long long num){
	//return Angle::FromDegrees(num);
	std::cout << "Constructing DEG from long " << num << std::endl;
	return Angle();
};
/* constexpr */ Angle operator "" _rad(long double num) {
	std::cout << "Constructing RAD from double " << num << std::endl;
	return Angle();
//  return Angle::FromRadians(num);
};
/* constexpr */ Angle operator "" _rad(unsigned long long num) {
	std::cout << "Constructing RAD from long " << num << std::endl;
	return Angle();
//  return Angle::FromRadians(num);
};
#if 0
constexpr Angle operator "" _deg(long double num){ return Angle::FromDegrees(num); };
constexpr Angle operator "" _deg(unsigned long long num){ return Angle::FromDegrees(num); };
constexpr Angle operator "" _rad(long double num){ return Angle::FromRadians(num); };
constexpr Angle operator "" _rad(unsigned long long num){ return Angle::FromRadians(num); };

//static_assert(90_deg == Degrees(90.0));
//static_assert(-90_deg == Degrees(-90.0));
#endif

std::ostream& operator<< (std::ostream& stream, const Angle obj);

