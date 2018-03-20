@echo off

echo "--------------------------------------------------------"
echo "1iter region select"
RELEASE-AMD64-CLR\NaiadSimulator.exe region select docs_1iter_500k-600kdocs_10-20auth_id10.in authors_1iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "1iter nonregion select"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion select docs_1iter_500k-600kdocs_10-20auth_id10.in authors_1iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "1iter region aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe region aggregate docs_1iter_500k-600kdocs_10-20auth_id10.in authors_1iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "1iter nonregion aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion aggregate docs_1iter_500k-600kdocs_10-20auth_id10.in authors_1iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "1iter region join"
RELEASE-AMD64-CLR\NaiadSimulator.exe region join docs_1iter_500k-600kdocs_10-20auth_id10.in authors_1iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "1iter nonregion join"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion join docs_1iter_500k-600kdocs_10-20auth_id10.in authors_1iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "1iter region windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe region windowjoin docs_1iter_500k-600kdocs_10-20auth_id10.in authors_1iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "1iter nonregion windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion windowjoin docs_1iter_500k-600kdocs_10-20auth_id10.in authors_1iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "1iter region groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe region groupby docs_1iter_500k-600kdocs_10-20auth_id10.in authors_1iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "1iter nonregion groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion groupby docs_1iter_500k-600kdocs_10-20auth_id10.in authors_1iter_500k-600kdocs_10-20auth_id10.in





echo "--------------------------------------------------------"
echo "2iter region select"
RELEASE-AMD64-CLR\NaiadSimulator.exe region select docs_2iter_500k-600kdocs_10-20auth_id10.in authors_2iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "2iter nonregion select"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion select docs_2iter_500k-600kdocs_10-20auth_id10.in authors_2iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "2iter region aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe region aggregate docs_2iter_500k-600kdocs_10-20auth_id10.in authors_2iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "2iter nonregion aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion aggregate docs_2iter_500k-600kdocs_10-20auth_id10.in authors_2iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "2iter region join"
RELEASE-AMD64-CLR\NaiadSimulator.exe region join docs_2iter_500k-600kdocs_10-20auth_id10.in authors_2iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "2iter nonregion join"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion join docs_2iter_500k-600kdocs_10-20auth_id10.in authors_2iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "2iter region windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe region windowjoin docs_2iter_500k-600kdocs_10-20auth_id10.in authors_2iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "2iter nonregion windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion windowjoin docs_2iter_500k-600kdocs_10-20auth_id10.in authors_2iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "2iter region groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe region groupby docs_2iter_500k-600kdocs_10-20auth_id10.in authors_2iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "2iter nonregion groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion groupby docs_2iter_500k-600kdocs_10-20auth_id10.in authors_2iter_500k-600kdocs_10-20auth_id10.in





echo "--------------------------------------------------------"
echo "4iter region select"
RELEASE-AMD64-CLR\NaiadSimulator.exe region select docs_4iter_500k-600kdocs_10-20auth_id10.in authors_4iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "4iter nonregion select"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion select docs_4iter_500k-600kdocs_10-20auth_id10.in authors_4iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "4iter region aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe region aggregate docs_4iter_500k-600kdocs_10-20auth_id10.in authors_4iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "4iter nonregion aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion aggregate docs_4iter_500k-600kdocs_10-20auth_id10.in authors_4iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "4iter region join"
RELEASE-AMD64-CLR\NaiadSimulator.exe region join docs_4iter_500k-600kdocs_10-20auth_id10.in authors_4iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "4iter nonregion join"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion join docs_4iter_500k-600kdocs_10-20auth_id10.in authors_4iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "4iter region windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe region windowjoin docs_4iter_500k-600kdocs_10-20auth_id10.in authors_4iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "4iter nonregion windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion windowjoin docs_4iter_500k-600kdocs_10-20auth_id10.in authors_4iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "4iter region groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe region groupby docs_4iter_500k-600kdocs_10-20auth_id10.in authors_4iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "4iter nonregion groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion groupby docs_4iter_500k-600kdocs_10-20auth_id10.in authors_4iter_500k-600kdocs_10-20auth_id10.in




echo "--------------------------------------------------------"
echo "8iter region select"
RELEASE-AMD64-CLR\NaiadSimulator.exe region select docs_8iter_500k-600kdocs_10-20auth_id10.in authors_8iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "8iter nonregion select"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion select docs_8iter_500k-600kdocs_10-20auth_id10.in authors_8iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "8iter region aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe region aggregate docs_8iter_500k-600kdocs_10-20auth_id10.in authors_8iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "8iter nonregion aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion aggregate docs_8iter_500k-600kdocs_10-20auth_id10.in authors_8iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "8iter region join"
RELEASE-AMD64-CLR\NaiadSimulator.exe region join docs_8iter_500k-600kdocs_10-20auth_id10.in authors_8iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "8iter nonregion join"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion join docs_8iter_500k-600kdocs_10-20auth_id10.in authors_8iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "8iter region windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe region windowjoin docs_8iter_500k-600kdocs_10-20auth_id10.in authors_8iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "8iter nonregion windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion windowjoin docs_8iter_500k-600kdocs_10-20auth_id10.in authors_8iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "8iter region groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe region groupby docs_8iter_500k-600kdocs_10-20auth_id10.in authors_8iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "8iter nonregion groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion groupby docs_8iter_500k-600kdocs_10-20auth_id10.in authors_8iter_500k-600kdocs_10-20auth_id10.in



echo "--------------------------------------------------------"
echo "12iter region select"
RELEASE-AMD64-CLR\NaiadSimulator.exe region select docs_12iter_500k-600kdocs_10-20auth_id10.in authors_12iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "12iter nonregion select"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion select docs_12iter_500k-600kdocs_10-20auth_id10.in authors_12iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "12iter region aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe region aggregate docs_12iter_500k-600kdocs_10-20auth_id10.in authors_12iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "12iter nonregion aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion aggregate docs_12iter_500k-600kdocs_10-20auth_id10.in authors_12iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "12iter region join"
RELEASE-AMD64-CLR\NaiadSimulator.exe region join docs_12iter_500k-600kdocs_10-20auth_id10.in authors_12iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "12iter nonregion join"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion join docs_12iter_500k-600kdocs_10-20auth_id10.in authors_12iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "12iter region windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe region windowjoin docs_12iter_500k-600kdocs_10-20auth_id10.in authors_12iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "12iter nonregion windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion windowjoin docs_12iter_500k-600kdocs_10-20auth_id10.in authors_12iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "12iter region groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe region groupby docs_12iter_500k-600kdocs_10-20auth_id10.in authors_12iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "12iter nonregion groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion groupby docs_12iter_500k-600kdocs_10-20auth_id10.in authors_12iter_500k-600kdocs_10-20auth_id10.in





echo "--------------------------------------------------------"
echo "16iter region select"
RELEASE-AMD64-CLR\NaiadSimulator.exe region select docs_16iter_500k-600kdocs_10-20auth_id10.in authors_16iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "16iter nonregion select"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion select docs_16iter_500k-600kdocs_10-20auth_id10.in authors_16iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "16iter region aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe region aggregate docs_16iter_500k-600kdocs_10-20auth_id10.in authors_16iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "16iter nonregion aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion aggregate docs_16iter_500k-600kdocs_10-20auth_id10.in authors_16iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "16iter region join"
RELEASE-AMD64-CLR\NaiadSimulator.exe region join docs_16iter_500k-600kdocs_10-20auth_id10.in authors_16iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "16iter nonregion join"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion join docs_16iter_500k-600kdocs_10-20auth_id10.in authors_16iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "16iter region windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe region windowjoin docs_16iter_500k-600kdocs_10-20auth_id10.in authors_16iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "16iter nonregion windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion windowjoin docs_16iter_500k-600kdocs_10-20auth_id10.in authors_16iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "16iter region groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe region groupby docs_16iter_500k-600kdocs_10-20auth_id10.in authors_16iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "16iter nonregion groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion groupby docs_16iter_500k-600kdocs_10-20auth_id10.in authors_16iter_500k-600kdocs_10-20auth_id10.in




echo "--------------------------------------------------------"
echo "20iter region select"
RELEASE-AMD64-CLR\NaiadSimulator.exe region select docs_20iter_500k-600kdocs_10-20auth_id10.in authors_20iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "20iter nonregion select"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion select docs_20iter_500k-600kdocs_10-20auth_id10.in authors_20iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "20iter region aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe region aggregate docs_20iter_500k-600kdocs_10-20auth_id10.in authors_20iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "20iter nonregion aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion aggregate docs_20iter_500k-600kdocs_10-20auth_id10.in authors_20iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "20iter region join"
RELEASE-AMD64-CLR\NaiadSimulator.exe region join docs_20iter_500k-600kdocs_10-20auth_id10.in authors_20iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "20iter nonregion join"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion join docs_20iter_500k-600kdocs_10-20auth_id10.in authors_20iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "20iter region windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe region windowjoin docs_20iter_500k-600kdocs_10-20auth_id10.in authors_20iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "20iter nonregion windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion windowjoin docs_20iter_500k-600kdocs_10-20auth_id10.in authors_20iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "20iter region groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe region groupby docs_20iter_500k-600kdocs_10-20auth_id10.in authors_20iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "20iter nonregion groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion groupby docs_20iter_500k-600kdocs_10-20auth_id10.in authors_20iter_500k-600kdocs_10-20auth_id10.in



echo "--------------------------------------------------------"
echo "24iter region select"
RELEASE-AMD64-CLR\NaiadSimulator.exe region select docs_24iter_500k-600kdocs_10-20auth_id10.in authors_24iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "24iter nonregion select"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion select docs_24iter_500k-600kdocs_10-20auth_id10.in authors_24iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "24iter region aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe region aggregate docs_24iter_500k-600kdocs_10-20auth_id10.in authors_24iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "24iter nonregion aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion aggregate docs_24iter_500k-600kdocs_10-20auth_id10.in authors_24iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "24iter region join"
RELEASE-AMD64-CLR\NaiadSimulator.exe region join docs_24iter_500k-600kdocs_10-20auth_id10.in authors_24iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "24iter nonregion join"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion join docs_24iter_500k-600kdocs_10-20auth_id10.in authors_24iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "24iter region windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe region windowjoin docs_24iter_500k-600kdocs_10-20auth_id10.in authors_24iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "24iter nonregion windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion windowjoin docs_24iter_500k-600kdocs_10-20auth_id10.in authors_24iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "24iter region groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe region groupby docs_24iter_500k-600kdocs_10-20auth_id10.in authors_24iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "24iter nonregion groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion groupby docs_24iter_500k-600kdocs_10-20auth_id10.in authors_24iter_500k-600kdocs_10-20auth_id10.in



echo "--------------------------------------------------------"
echo "28iter region select"
RELEASE-AMD64-CLR\NaiadSimulator.exe region select docs_28iter_500k-600kdocs_10-20auth_id10.in authors_28iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "28iter nonregion select"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion select docs_28iter_500k-600kdocs_10-20auth_id10.in authors_28iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "28iter region aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe region aggregate docs_28iter_500k-600kdocs_10-20auth_id10.in authors_28iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "28iter nonregion aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion aggregate docs_28iter_500k-600kdocs_10-20auth_id10.in authors_28iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "28iter region join"
RELEASE-AMD64-CLR\NaiadSimulator.exe region join docs_28iter_500k-600kdocs_10-20auth_id10.in authors_28iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "28iter nonregion join"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion join docs_28iter_500k-600kdocs_10-20auth_id10.in authors_28iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "28iter region windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe region windowjoin docs_28iter_500k-600kdocs_10-20auth_id10.in authors_28iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "28iter nonregion windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion windowjoin docs_28iter_500k-600kdocs_10-20auth_id10.in authors_28iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "28iter region groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe region groupby docs_28iter_500k-600kdocs_10-20auth_id10.in authors_28iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "28iter nonregion groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion groupby docs_28iter_500k-600kdocs_10-20auth_id10.in authors_28iter_500k-600kdocs_10-20auth_id10.in




echo "--------------------------------------------------------"
echo "32iter region select"
RELEASE-AMD64-CLR\NaiadSimulator.exe region select docs_32iter_500k-600kdocs_10-20auth_id10.in authors_32iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "32iter nonregion select"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion select docs_32iter_500k-600kdocs_10-20auth_id10.in authors_32iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "32iter region aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe region aggregate docs_32iter_500k-600kdocs_10-20auth_id10.in authors_32iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "32iter nonregion aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion aggregate docs_32iter_500k-600kdocs_10-20auth_id10.in authors_32iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "32iter region join"
RELEASE-AMD64-CLR\NaiadSimulator.exe region join docs_32iter_500k-600kdocs_10-20auth_id10.in authors_32iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "32iter nonregion join"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion join docs_32iter_500k-600kdocs_10-20auth_id10.in authors_32iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "32iter region windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe region windowjoin docs_32iter_500k-600kdocs_10-20auth_id10.in authors_32iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "32iter nonregion windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion windowjoin docs_32iter_500k-600kdocs_10-20auth_id10.in authors_32iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "32iter region groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe region groupby docs_32iter_500k-600kdocs_10-20auth_id10.in authors_32iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "32iter nonregion groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion groupby docs_32iter_500k-600kdocs_10-20auth_id10.in authors_32iter_500k-600kdocs_10-20auth_id10.in





echo "--------------------------------------------------------"
echo "36iter region select"
RELEASE-AMD64-CLR\NaiadSimulator.exe region select docs_36iter_500k-600kdocs_10-20auth_id10.in authors_36iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "36iter nonregion select"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion select docs_36iter_500k-600kdocs_10-20auth_id10.in authors_36iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "36iter region aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe region aggregate docs_36iter_500k-600kdocs_10-20auth_id10.in authors_36iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "36iter nonregion aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion aggregate docs_36iter_500k-600kdocs_10-20auth_id10.in authors_36iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "36iter region join"
RELEASE-AMD64-CLR\NaiadSimulator.exe region join docs_36iter_500k-600kdocs_10-20auth_id10.in authors_36iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "36iter nonregion join"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion join docs_36iter_500k-600kdocs_10-20auth_id10.in authors_36iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "36iter region windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe region windowjoin docs_36iter_500k-600kdocs_10-20auth_id10.in authors_36iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "36iter nonregion windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion windowjoin docs_36iter_500k-600kdocs_10-20auth_id10.in authors_36iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "36iter region groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe region groupby docs_36iter_500k-600kdocs_10-20auth_id10.in authors_36iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "36iter nonregion groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion groupby docs_36iter_500k-600kdocs_10-20auth_id10.in authors_36iter_500k-600kdocs_10-20auth_id10.in





echo "--------------------------------------------------------"
echo "40iter region select"
RELEASE-AMD64-CLR\NaiadSimulator.exe region select docs_40iter_500k-600kdocs_10-20auth_id10.in authors_40iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "40iter nonregion select"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion select docs_40iter_500k-600kdocs_10-20auth_id10.in authors_40iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "40iter region aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe region aggregate docs_40iter_500k-600kdocs_10-20auth_id10.in authors_40iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "40iter nonregion aggregate"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion aggregate docs_40iter_500k-600kdocs_10-20auth_id10.in authors_40iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "40iter region join"
RELEASE-AMD64-CLR\NaiadSimulator.exe region join docs_40iter_500k-600kdocs_10-20auth_id10.in authors_40iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "40iter nonregion join"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion join docs_40iter_500k-600kdocs_10-20auth_id10.in authors_40iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "40iter region windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe region windowjoin docs_40iter_500k-600kdocs_10-20auth_id10.in authors_40iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "40iter nonregion windowjoin"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion windowjoin docs_40iter_500k-600kdocs_10-20auth_id10.in authors_40iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "40iter region groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe region groupby docs_40iter_500k-600kdocs_10-20auth_id10.in authors_40iter_500k-600kdocs_10-20auth_id10.in

echo "--------------------------------------------------------"
echo "40iter nonregion groupby"
RELEASE-AMD64-CLR\NaiadSimulator.exe nonregion groupby docs_40iter_500k-600kdocs_10-20auth_id10.in authors_40iter_500k-600kdocs_10-20auth_id10.in
