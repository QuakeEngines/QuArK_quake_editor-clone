#ifndef TPIXEL_H
#define TPIXEL_H

#include <math.h>
#include <memory.h>
#include <dds/tVector.h>

// modulo value x between [lo,hi]
// allows value 'hi'
template < class _Type > 
inline _Type Clamp(const _Type &x, 
                   const _Type &lo, 
                   const _Type &hi)
{
    if (x < lo)
        return lo;
    else if (x > hi)
        return hi;
    else
        return x;
}


inline int iClamp(int x, int lo, int hi)
{
    if (x < lo)
        return lo;
    if (x > hi)
        return hi;
    return x;
}

inline float fClamp(float x, float lo, float hi)
{
    if (x < lo)
        return lo;
    if (x > hi)
        return hi;
    return x;
}


inline int fmod(int x, int size)
{
    return x % size;
}

inline __int64 fmod(__int64 x, __int64 size)
{
    return x % size;
}
inline unsigned __int64 fmod(unsigned __int64 x, unsigned __int64 size)
{
    return x % size;
}



// lo = 0;
// allow hi value
template < class _Type > 
inline _Type Modulo(const _Type &x, const _Type &hi)
{
    if (x >= 0 && x <= hi)
        return x;

    _Type f = fmod(x, hi);
    
    if (f < 0)
        f += hi;

    return f;
}
// does not allow x == size
inline int iModulo(int x, int size)
{
    if (x < 0)
    {
        int n = x / size;
        x += size * (n + 1);
    }
        
    return x % size;
}



template < class _Type > 
inline _Type Modulo(const _Type &x, const _Type &lo, const _Type &hi)
{
    if (x >= lo && x <= hi)
        return x;

    _Type dw = hi - lo;
    _Type t = x - lo;

    _Type f = fmod(t, dw);

    
    if (f < 0)
        f += dw;

    f += lo;

    return f;
}



#pragma pack(push,4)

// red and green
class v16u16_t	
{
public:

    union
    {
        short uv[4];
        struct
        {
            short u;
            short v;
        };
    };
    v16u16_t & operator += ( const v16u16_t & v );     // incrementation by a Vec4f


    void set(unsigned short _u, unsigned short _v)
    {
        u = _u;
        v = _v;
    }
};

class r12g12b8_t	
{
public:

    union
    {
        
        struct
        {
            unsigned long r:12;
            unsigned long g:12;
            unsigned long b:8;
        };
    };
    r12g12b8_t & operator += ( const r12g12b8_t& v );     // incrementation by a Vec4f


    void set(unsigned long _r, unsigned long _g, unsigned long _b)
    {
        r = _r;
        g = _g;
        b = _b;
    }
};



class rgba_t	
{
public:

    union
    {
        unsigned long u;
        unsigned char rgba[4];
        struct
        {
            unsigned char r;
            unsigned char g;
            unsigned char b;
            unsigned char a;
        };
    };

    rgba_t() 
    {
    }

    // 
    
    unsigned long bgra() 
    {
        
        return ((unsigned long)a << 24) | ((unsigned long)r << 16) | ((unsigned long)g << 8) | ((unsigned long)b);
    }
    rgba_t(unsigned char _r, unsigned char _g, unsigned char _b,unsigned char _a) 
    {
        a = _a; 
        r = _r; 
        g = _g;
        b = _b;
    }

    rgba_t & operator += ( const rgba_t& v )     // incrementation by a rgba_t
    {
        r = Clamp((int)((int)r + (int)v.r), 0, 255);   
        g = Clamp((int)g + (int)v.g, 0, 255);   
        b = Clamp((int)b + (int)v.b, 0, 255);   
        a = Clamp((int)a + (int)v.a, 0, 255);   

        return *this;
    }

    rgba_t & operator -= ( const rgba_t& v );     // decrementation by a rgba_t
    rgba_t & operator *= ( const float d );     // multiplication by a constant
    rgba_t & operator /= ( const float d );     // division by a constant


    rgba_t& operator = (const rgba_t& v)
    { 
        r = v.r; 
        g = v.g; 
        b = v.b; 
        a = v.a; 
        return *this; 
    }

    friend rgba_t operator + (const rgba_t & v1, const rgba_t& v2)
    {

        int r,g,b,a;
        r = Clamp((int)v1.r + (int)v2.r, 0, 255);   
        g = Clamp((int)v1.g + (int)v2.g, 0, 255);   
        b = Clamp((int)v1.b + (int)v2.b, 0, 255);   
        a = Clamp((int)v1.a + (int)v2.a, 0, 255);  

        return rgba_t(r, g, b, a);
    }

    friend rgba_t operator / (const rgba_t& v, float s)
    {
        return rgba_t(v.r/s, v.g/s, v.b/s, v.a/s);
    }

    friend rgba_t operator / (const rgba_t& v, int s)
    {
        return rgba_t(v.r/s, v.g/s, v.b/s, v.a/s);
    }

    void set(unsigned char _r, unsigned char _g, unsigned char _b, unsigned char _a)
    {
        r = _r;
        g = _g;
        b = _b;
        a = _a;
    }

    void SetToZero()
    {
        r = g = b = a = 0;
    }
};

class rgba16_t	
{
public:

    union
    {
        //unsigned __int64 u;
        unsigned short rgba[4];
        struct
        {
            unsigned short r;
            unsigned short g;
            unsigned short b;
            unsigned short a;
        };
    };

    rgba16_t() 
    {
    }
    rgba16_t(unsigned short _r, unsigned short _g, unsigned short _b,unsigned short _a) 
    {
        a = _a; 
        r = _r; 
        g = _g;
        b = _b;
    }

    rgba16_t & operator += ( const rgba16_t& v )     // incrementation by a rgba_t
    {
        r = Clamp((int)r + (int)v.r, 0, 65535);   
        g = Clamp((int)g + (int)v.g, 0, 65535);   
        b = Clamp((int)b + (int)v.b, 0, 65535);   
        a = Clamp((int)a + (int)v.a, 0, 65535);   

        return *this;
    }

    rgba16_t & operator -= ( const rgba16_t& v );     // decrementation by a rgba_t
    rgba16_t & operator *= ( const float d );     // multiplication by a constant
    rgba16_t & operator /= ( const float d );     // division by a constant


    rgba16_t& operator = (const rgba16_t& v)
    { 
        r = v.r; 
        g = v.g; 
        b = v.b; 
        a = v.a; 
        return *this; 
    }

    friend rgba16_t operator + (const rgba16_t & v1, const rgba16_t& v2)
    {

        int r,g,b,a;
        r = Clamp((int)v1.r + (int)v2.r, 0, 65535);   
        g = Clamp((int)v1.g + (int)v2.g, 0, 65535);   
        b = Clamp((int)v1.b + (int)v2.b, 0, 65535);   
        a = Clamp((int)v1.a + (int)v2.a, 0, 65535);  

        return rgba16_t(r, g, b, a);
    }

    friend rgba16_t operator / (const rgba16_t& v, float s)
    {
        return rgba16_t(v.r/s, v.g/s, v.b/s, v.a/s);
    }

    friend rgba16_t operator / (const rgba16_t& v, int s)
    {
        return rgba16_t(v.r/s, v.g/s, v.b/s, v.a/s);
    }

    void set(unsigned short _r, unsigned short _g, unsigned short _b, unsigned short _a)
    {
        r = _r;
        g = _g;
        b = _b;
        a = _a;
    }
};


class urgba_t	
{
public:

    union
    {
        unsigned long u;
        char rgba[4];
        struct
        {
            char r;
            char g;
            char b;
            char a;
        };
    };
    urgba_t & operator += ( const urgba_t& v );     // incrementation by a Vec4f


    void set(char _r, char _g, char _b, char _a)
    {
        r = _r;
        g = _g;
        b = _b;
        a = _a;
    }
};


 

class q8w8v8u8_t	
{
public:

    union
    {
        char qwvu[4];
        struct
        {
            char q;
            char w;
            char v;
            char u;
        };
    };
    q8w8v8u8_t & operator += ( const q8w8v8u8_t& v );     // incrementation by a Vec4f


    void set(char _r, char _g, char _b, char _a)
    {
        q = _r;
        w = _g;
        v = _b;
        u = _a;
    }
};







#define _R 0
#define _G 1
#define _B 2
#define _A 3




class fpPixel
{
public:
    union
    {
        float p[4];
        struct
        {
            float r;
            float g;
            float b;
            float a;
        };
        struct
        {
            float x;
            float y;
            float z;
            float w;
        };

    };

    void SetToZero()
    {
        r = 0;
        g = 0;
        b = 0;
        a = 0;

    }


    void Clamp(fpPixel & lo, fpPixel & hi)
    {

        r = ::Clamp(r, lo.r, hi.r);
        g = ::Clamp(g, lo.g, hi.g);
        b = ::Clamp(b, lo.b, hi.b);
        a = ::Clamp(a, lo.a, hi.a);
    }

    void Wrap(fpPixel & lo, fpPixel & hi)
    {

        r = Modulo(r, lo.r, hi.r);
        g = Modulo(g, lo.g, hi.g);
        b = Modulo(b, lo.b, hi.b);
        a = Modulo(a, lo.a, hi.a);


    }


    //float& operator () ( int i) { return p[i]; };       // indexing
    //const float& operator()(int i) const { return p[i];}



    fpPixel() {}
    fpPixel(const float _r, const float _g, const float _b, const float _a) 
    {
        a = _a; 
        r = _r; 
        g = _g;
        b = _b;
    }


    fpPixel(const fpPixel& v)
    {
        a = v.a; 
        r = v.r; 
        g = v.g;
        b = v.b;
    }          // copy constructor

    void set(const float _r, const float _g, const float _b, const float _a)
    {
        a = _a; 
        r = _r; 
        g = _g;
        b = _b;
    }

    void set(const fpPixel& v)
    {
        a = v.a; 
        r = v.r; 
        g = v.g;
        b = v.b;
    }


    fpPixel & operator += ( const fpPixel& v )     // incrementation by a rgba_t
    {
        r += v.r;   
        g += v.g;   
        b += v.b;   
        a += v.a;   

        return *this;
    }


    fpPixel & operator -= ( const fpPixel& v )     // incrementation by a rgba_t
    {
        r -= v.r;   
        g -= v.g;   
        b -= v.b;   
        a -= v.a;   

        return *this;
    }


    fpPixel & operator *= ( const fpPixel& v )     // incrementation by a rgba_t
    {
        r *= v.r;   
        g *= v.g;   
        b *= v.b;   
        a *= v.a;   

        return *this;
    }

    fpPixel & operator /= ( const fpPixel& v )     // incrementation by a rgba_t
    {
        r /= v.r;   
        g /= v.g;   
        b /= v.b;   
        a /= v.a;   

        return *this;
    }


    fpPixel & operator = ( const fpPixel& v );      // assignment of a Vec3f         



    friend fpPixel operator + (const fpPixel& v1, const fpPixel& v2)
    {
        return fpPixel(v1.r + v2.r, v1.g + v2.g, v1.b + v2.b, v1.a + v2.a);
    }

    friend fpPixel operator / (const fpPixel& v, float s)
    {
        return fpPixel(v.r/s, v.g/s, v.b/s, v.a/s);

    }
    friend int operator == (const fpPixel& v1, const fpPixel& v2);      // v1 == v2 ?

    int normalize()
    {
        double u;
        u = x * x + y * y + z * z;

        if ( fabs(u - 1.0) < 1e-12)
            return 0; // already normalized

        if ( fabs((double)u) < 1e-12)
        {
            x = y = z = 0.0;
            return -1;
        }


        u = 1.0 / sqrt(u);


        x *= u;
        y *= u;
        z *= u;

        return 0;
    }


};



class fpPixel3
{
public:
    union
    {
        float p[3];
        struct
        {
            float r;
            float g;
            float b;
        };
        struct
        {
            float x;
            float y;
            float z;
        };

    };


    void SetToZero()
    {
        r = 0;
        g = 0;
        b = 0;
    }
    
    //float& operator () ( int i) { return p[i]; };       // indexing
    //const float& operator()(int i) const { return p[i];}



    fpPixel3() {}
    fpPixel3(const float _r, const float _g, const float _b) 
    {
        r = _r; 
        g = _g;
        b = _b;
    }


    fpPixel3(const fpPixel3& v)
    {
        r = v.r; 
        g = v.g;
        b = v.b;
    }          // copy constructor

    void set(const float _r, const float _g, const float _b)
    {
        r = _r; 
        g = _g;
        b = _b;
    }

    void set(const fpPixel3& v)
    {
        r = v.r; 
        g = v.g;
        b = v.b;
    }

    fpPixel3 & operator += ( const fpPixel3& v );     // incrementation by a Vec4f

    fpPixel3 & operator = ( const fpPixel3& v );      // assignment of a Vec3f         
    fpPixel3 & operator -= ( const fpPixel3& v );     // decrementation by a Vec3f
    fpPixel3 & operator *= ( const float d );     // multiplication by a constant
    fpPixel3 & operator /= ( const float d );     // division by a constant



    friend fpPixel3 operator + (const fpPixel3& v1, const fpPixel3& v2)
    {
        return fpPixel3(v1.r + v2.r, v1.g + v2.g, v1.b + v2.b);
    }

    friend fpPixel3 operator / (const fpPixel3& v, float s)
    {
        return fpPixel3(v.r/s, v.g/s, v.b/s);

    }
    friend int operator == (const fpPixel3& v1, const fpPixel3& v2);      // v1 == v2 ?

    int normalize()
    {
        double u;
        u = x * x + y * y + z * z;

        if ( fabs(u - 1.0) < 1e-12)
            return 0; // already normalized

        if ( fabs((double)u) < 1e-12)
        {
            x = y = z = 0.0;
            return -1;
        }


        u = 1.0 / sqrt(u);


        x *= u;
        y *= u;
        z *= u;

        return 0;
    }


};


typedef fpPixel * fp_i;


inline int operator == (const fpPixel& v1, const fpPixel& v2)
{
    return 
        v1.a == v2.a && 
        v1.r == v2.r && 
        v1.b == v2.g && 
        v1.g == v2.b;
}

inline fpPixel& fpPixel::operator = (const fpPixel& v)
{ 
    a = v.a; 
    r = v.r; 
    g = v.g; 
    b = v.b; 
    return *this; 
}







template <class _type>
class nvImage 
{
    size_t m_width;
    size_t m_height;
    nvVector<_type> m_pixels;

public:
    size_t size()
    {
        return m_width * m_height;
    }
    

    nvImage < _type > & operator = ( const nvImage < _type >& v ) 
    {

        // resize and copy over
        resize(v.width(), v.height());

        m_pixels = v.m_pixels;

        return *this; 
    }


    _type& operator [] ( size_t i) 
    {
#ifdef _DEBUG
        assert(i < m_width * m_height);
#endif
        return m_pixels[i]; 
    };  
    
    const _type& operator[](size_t i) const 
    { 
#ifdef _DEBUG
        assert(i < m_width * m_height);
#endif
        return m_pixels[i];
    }

    _type & operator () (const size_t &y, const size_t &x) 
    {
#if _DEBUG
        assert(y < m_height);
        assert(x < m_width);
#endif
        return m_pixels[y * m_width + x]; 

    }

    int nPlanesInFile;

    size_t width() const
    {
        return m_width;

    }

    size_t height() const
    {
        return m_height;

    }



    _type * pixels(size_t n = 0)
    {
        
        return &m_pixels[n];
    }
    _type * pixelsXY(size_t x, size_t y)
    {
        return &m_pixels[y * width() + x];
    }
    _type * pixelsXY_Safe(size_t x, size_t y)
    {
        if (m_pixels.size() == 0)
            return 0;
        else
            return &m_pixels[y * width() + x];
    }

    _type * pixelsYX(size_t y, size_t x)
    {
        return &m_pixels[y * width() + x];

    }
    // row / column
    _type * pixelsRC(size_t y, size_t x)
    {
        return &m_pixels[y * width() + x];

    }


    _type & pixel_ref(size_t n = 0)
    {
        return m_pixels[n];
    }
    _type & pixelsXY_ref(size_t x, size_t y)
    {
        return m_pixels[y * width() + x];
    }

    _type & pixelsYX_ref(size_t y, size_t x)
    {
        return m_pixels[y * width() + x];

    }
    // row / column
    _type & pixelsRC_ref(size_t y, size_t x)
    {
        return m_pixels[y * width() + x];

    }

    _type * pixelsXY_wrapped(int x, int y)
    {
        y = mod(y, m_height);
        x = mod(x, m_width);

        return &m_pixels[y * m_width + x];
    }



    nvImage( const nvImage < _type > & other)
    {
        m_width = other.m_width;
        m_height = other.m_height;
        nPlanesInFile = other.nPlanesInFile;

        m_pixels = other.m_pixels;


    }

    nvImage()
    {
        m_width = 0;
        m_height = 0;

        nPlanesInFile = -1;

        m_pixels.clear();
        
    };
   ~nvImage()
    {
 
    }
    void clear()
    {
        m_width = 0;
        m_height = 0;
        m_pixels.clear();
    }

    void resize(size_t width, size_t height)
    {
        m_pixels.resize(width * height);
        m_width = width;
        m_height = height;

    }

    nvImage<_type>(size_t width, size_t height)
    {
        m_pixels.resize(width * height);
        m_width = width;
        m_height = height;
        nPlanesInFile = -1;

    };


    void SwapRB()
    {
		
        _type * p = &m_pixels[0];
		for(size_t i=0; i < m_width * m_height; i++ );
		{

            int r = p->r;
            p->r = p->b;
            p->b = r;
		} 
    }


    void	FlipTopToBottom()
    {

        _type * swap = new _type[ m_width];

        size_t row;

        _type * end_row;
        _type * start_row;

        size_t len = sizeof(_type) * m_width;

        for( row = 0; row < m_height / 2; row ++ )
        {
            end_row =   &m_pixels[ m_width * ( m_height - row - 1) ];
            start_row = &m_pixels[ m_width * row ];

            // copy row toward end of image into temporary swap buffer
            memcpy( swap, end_row, len );

            // copy row at beginning to row at end
            memcpy( end_row, start_row, len );

            // copy old bytes from row at end (in swap) to row at beginning
            memcpy( start_row, swap, len );
        }

        delete [] swap;
    }

    void SetToZero()
    {

        for(size_t i=0; i< m_width * m_height; i++)
        {
            pixel_ref(i).SetToZero();

        }
    }
    void SetToZeroDirect()
    {
        for(size_t i=0; i< m_width * m_height; i++)
        {
            m_pixels[i] = 0;

        }
    }


 
};




typedef nvImage<rgba_t> RGBAImage;


typedef nvMatrix<fpPixel> fpImage;
typedef nvMatrix<fpPixel3> fpImage3;



#pragma pack(pop)



#include <dds/ConvertColor.h>   


#endif