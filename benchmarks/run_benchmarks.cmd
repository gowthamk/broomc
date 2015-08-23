@echo off

echo "--------------------------------------------------------"
echo "Compiling allocation overhead micro-benchmark"
echo "--------------------------------------------------------"
bc /runrelease /midori /release /clr BenchmarkAllocationOverhead.cs

echo "--------------------------------------------------------"
echo "Running allocation overhead micro-benchmark"
echo "--------------------------------------------------------"
RELEASE-AMD64-CLR\BenchmarkAllocationOverhead.exe 4096 10

echo "--------------------------------------------------------"
echo "Compiling context creation micro-benchmark"
echo "--------------------------------------------------------"
bc /runrelease /midori /release /clr BenchmarkContextCreation.cs

echo "--------------------------------------------------------"
echo "Running context creation micro-benchmark"
echo "--------------------------------------------------------"
RELEASE-AMD64-CLR\BenchmarkContextCreation.exe 10 10

echo "--------------------------------------------------------"
echo "Compiling free region micro-benchmark"
echo "--------------------------------------------------------"
bc /runrelease /midori /release /clr BenchmarkFreeRegion.cs

echo "--------------------------------------------------------"
echo "Running free region micro-benchmark"
echo "--------------------------------------------------------"
RELEASE-AMD64-CLR\BenchmarkFreeRegion.exe 4096 10

echo "--------------------------------------------------------"
echo "Compiling region creation micro-benchmark"
echo "--------------------------------------------------------"
bc /runrelease /midori /release /clr BenchmarkRegionCreation.cs

echo "--------------------------------------------------------"
echo "Running region creation micro-benchmark"
echo "--------------------------------------------------------"
RELEASE-AMD64-CLR\BenchmarkRegionCreation.exe 4096 10

echo "--------------------------------------------------------"
echo "Compiling collection synthetic benchmark"
echo "--------------------------------------------------------"
bc /runrelease /midori /release /clr BenchmarkCollections.cs WarmHeap.cs

echo "--------------------------------------------------------"
echo "Running collection synthetic nonregion list benchmark"
echo "--------------------------------------------------------"
RELEASE-AMD64-CLR\BenchmarkCollections.exe nonregion 10 10 list

echo "--------------------------------------------------------"
echo "Running collection synthetic region list benchmark"
echo "--------------------------------------------------------"
RELEASE-AMD64-CLR\BenchmarkCollections.exe region 10 10 list

echo "--------------------------------------------------------"
echo "Running collection synthetic regioncontext list benchmark"
echo "--------------------------------------------------------"
RELEASE-AMD64-CLR\BenchmarkCollections.exe regioncontext 10 10 list

echo "--------------------------------------------------------"
echo "Running collection synthetic nonregion dictionary benchmark"
echo "--------------------------------------------------------"
RELEASE-AMD64-CLR\BenchmarkCollections.exe nonregion 10 10 dict

echo "--------------------------------------------------------"
echo "Running collection synthetic region dictionary benchmark"
echo "--------------------------------------------------------"
RELEASE-AMD64-CLR\BenchmarkCollections.exe region 10 10 dict

echo "--------------------------------------------------------"
echo "Running collection synthetic regioncontext dictionary benchmark"
echo "--------------------------------------------------------"
RELEASE-AMD64-CLR\BenchmarkCollections.exe regioncontext 10 10 dict

echo "--------------------------------------------------------"
echo "Compiling single object type benchmark"
echo "--------------------------------------------------------"
bc /runrelease /midori /release /clr BenchmarkSingleObject.cs WarmHeap.cs

echo "--------------------------------------------------------"
echo "Compiling single object type on heap benchmark"
echo "--------------------------------------------------------"
RELEASE-AMD64-CLR\BenchmarkSingleObject.exe nonregion 10000 10

echo "--------------------------------------------------------"
echo "Compiling single object type on region benchmark"
echo "--------------------------------------------------------"
RELEASE-AMD64-CLR\BenchmarkSingleObject.exe region 10000 10

echo "--------------------------------------------------------"
echo "Compiling single object type using region context benchmark"
echo "--------------------------------------------------------"
RELEASE-AMD64-CLR\BenchmarkSingleObject.exe regioncontext 10000 10
