import mapbox_vector_tile
import timeit

def wrapper(func, *args, **kwargs):
    def wrapped():
        return func(*args, **kwargs)
    return wrapped

def decodeIt(data):
    return mapbox_vector_tile.decode(data, y_coord_down=True)

def encodeIt(obj):
    return mapbox_vector_tile.encode([obj], y_coord_down=True)

# Benchmark the decoding process.
def benchDecode(file):
    print('Benchmarking {}'.format(file))

    with open(file, 'rb') as f:
        data = f.read()

    iters = 100
    wrapped = wrapper(decodeIt, data)
    the_time = timeit.timeit(wrapped, number=iters)

    print('Average: {} ms'.format(1000 * the_time / iters))

def benchEncode(obj, name):
    print('Benchmarking {}'.format(name))

    iters = 100
    wrapped = wrapper(encodeIt, obj)
    the_time = timeit.timeit(wrapped, number=iters)

    print('Average: {} ms'.format(1000 * the_time / iters))

print('*** DECODING ***')

benchDecode('test/onepoint.mvt')
benchDecode('test/linestring.mvt')
benchDecode('test/polygon.mvt')
benchDecode('test/roads.mvt')

onepoint = {
    "name": "OnePoint",
    "features": [
        {
            "geometry":"POINT(5 5)",
            "properties": {}
        }
    ]
}

linestring = {
    "name": "OneLineString",
    "features": [
        {
            "geometry":"LINESTRING(5 5, 1200 1200)",
            "properties": {}
        }
    ]
}

polygon = {
    "name": "OnePolygon",
    "features": [
        {
            "geometry":"POLYGON ((2 2, 5 4, 2 6, 2 2))",
            "properties": {}
        }
    ]
}

print('*** ENCODING ***')

benchEncode(onepoint, 'One Point')
benchEncode(linestring, 'One LineString')
benchEncode(polygon, 'One Polygon')

print("Done")
